{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Automatic three-way merge for ispell personal dictionaries.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental
-}

-- A few notes on implementation, based on this:
-- http://aspell.net/man-html/Format-of-the-Personal-and-Replacement-Dictionaries.html
--
-- 1. We don't need to worry about encodings, because aspell only uses
-- 8-bit encodings.  As long as \n == 0x0A, we should be good.  Thus,
-- we *don't* worry about encoding, and use raw ByteString's.
--
-- 2. The output doesn't need to be sorted.  I've done a quick test to
-- confirm this, and it seems to be true (good thing, since sorting
-- means dealing with encodings)
--
-- 3. My own custom fr dict doesn't parse as utf-8, hence the two
-- previous points.

module Main where

import Control.Exception (try)
import Control.Monad (when)
import Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set (..), difference, fromList, intersection, toList, union, (\\))
import GHC.IO.Handle.Types (Handle)
import Options.Applicative (Parser, argument, execParser, fullDesc, help, helper, info, long, progDesc, short, strOption, (<**>), metavar, str, optional)
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO (IOMode (..), hPutStrLn, openFile, putStrLn, stderr, stdout, hClose)
import Prelude hiding (hPutStrLn, putStrLn, readFile)
import Data.String (fromString)

-- * Types

data Invocation = Invocation
  { invocO :: FilePath,
    invocA :: FilePath,
    invocB :: FilePath,
    invocOut :: Maybe FilePath
  }

data Header = Header
  { hVersion :: ByteString,
    hLocale :: ByteString,
    hEncoding :: ByteString
  }
  deriving (Show)

data Dict a = Dict
  { dHeader :: Header,
    dElems :: Set a
  }
  deriving (Show)

-- * Main functions

-- ** Merge

-- | Merge sorted lists from a common ancestor.  The resulting list is
-- made of the following elements:
--  - Those present in both a and b (their intersection)
--  - Elements absent from o, but present in a xor in b.
--
-- (Elements present in o, but absent from a or b are assumed to
-- have been deleted)
mergeSets :: (Ord a) => Set a -> Set a -> Set a -> Set a
mergeSets o a b =
  let rule1 = intersection a b
      rule2 = (a `symdiff` b) \\ o
   in rule1 `union` rule2

merge :: (Ord a) => Dict a -> Dict a -> Dict a -> Either String (Dict a)
merge o a b =
  do
    check o a b
    return $ Dict (dHeader b) (mergeSets (dElems o) (dElems a) (dElems b))

-- ** Safety checks

type Check a = Dict a -> Dict a -> Dict a -> Either String ()

checkEqual :: (Eq b) => String -> (Dict a -> b) -> Check a
checkEqual err f o a b
  | f o == f a && f a == f b = Right ()
  | otherwise = Left err

check :: Dict a -> Dict a -> Dict a -> Either String ()
check o a b = do
  checkEqual "aspell versions don't match" (hVersion . dHeader) o a b
  checkEqual "locales don't match" (hLocale . dHeader) o a b
  checkEqual "encodings don't match" (hEncoding . dHeader) o a b

-- * Readers and parsers

readDict :: [ByteString] -> Dict ByteString
readDict (x : xs) = Dict (parseHeader x) (fromList xs')
  where
    xs' = filter (mempty /=) xs
readDict [] = error "Empty file."

readDictFile :: FilePath -> IO (Dict ByteString)
readDictFile f = do
  raw <- BS.readFile f
  return $ readDict (BS.split 0x0A raw)

parseHeader :: ByteString -> Header
parseHeader s =
  let parts = BS.split 0x20 s
   in Header (head parts) (parts !! 1) (parts `nthOrMempty` 3)

-- | print an header with a given number of words.  Notice that the
-- number of words isn't required (see the doc linked above), but we
-- output it anyways (maybe aspell uses it to allocate memory, or
-- something)
printHeader :: Header -> Int -> ByteString
printHeader h c = hVersion h
                  <> " "
                  <> hLocale h
                  <> " "
                  <> (fromString . show $ c)
                  <> " "
                  <> hEncoding h

handleResult :: Handle -> Either String (Dict ByteString) -> IO ()
handleResult _ (Left err) = do
  hPutStrLn stderr ("Error: " <> err <> ".  Refusing to proceed.")
  exitWith $ ExitFailure 2
handleResult f (Right dict) =
  do
    BS8.hPutStrLn f (printDict dict)
    hClose f

-- * writer

printDict :: Dict ByteString -> ByteString
printDict (Dict h l) =
  let lines = printHeader h (length l) : toList l
   in BS.intercalate "\n" lines

-- * User interface

argParser :: Parser Invocation
argParser =
  Invocation
    <$> argument
      str
      ( metavar "ANCESTOR"
          <> help "Path to a common ancestor to both versions."
      )
      <*> argument
        str
        ( metavar "LEFT"
            <> help "Path to local revision."
        )
      <*> argument
        str
        ( metavar "THEIRS"
            <> help "Path to the other revision."
        )
      <*> ( optional $
              strOption $
                long "output"
                  <> short 'o'
                  <> help "Output (default stdout)")

main = main' =<< execParser opts
  where
    opts =
      info
        (argParser <**> helper)
        ( fullDesc
            <> progDesc "Automatic three-way merge for aspell custom dictionaries."
        )

main' :: Invocation -> IO ()
main' i = do
  o <- readDictFile (invocO i)
  a <- readDictFile (invocA i)
  b <- readDictFile (invocB i)
  outfd <-
    if isNothing (invocOut i)
      then return stdout
      else openFile (fromJust $ invocOut i) WriteMode

  handleResult outfd (merge o a b)

-- * Utilities

nthOrMempty :: (Monoid a) => [a] -> Int -> a
nthOrMempty xs n
  | length xs > n = xs !! n
  | otherwise = mempty

-- | Symmetric difference
symdiff :: (Ord a) => Set a -> Set a -> Set a
symdiff a b = (a `union` b) \\ (a `intersection` b)
