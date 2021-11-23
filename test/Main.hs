{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import IBAN                                                         as IBAN

import Control.Monad
import Data.ByteString.Char8                                        ( ByteString )
import Data.List                                                    ( sort )
import Data.Void
import System.Environment
import System.FilePath
import System.IO.Unsafe
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf
import qualified Data.ByteString.Char8                              as B8
import qualified Text.Megaparsec.Char.Lexer                         as L


main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain
    $ localOption (mkTimeout 20000000)                  -- timeout each test after 20 s
    $ testGroup "iban-calculator"
    [ testGroup "count"
      [ test_count 1 (Config 139483  928234   11 1) 71705
      , test_count 1 (Config 2824374 25823728 24 1) 958317
      , test_count 5 (Config 2824374 25823728 24 2) 958317
      , test_count 5 (Config 2824374 25823728 24 4) 958317
      , test_count 5 (Config 2824374 25823728 24 16) 958317
      , test_count 5 (Config 2824374 25823728 24 7) 958317
      , test_count 1 (Config 0 10 1 3) 10
      , test_count 1 (Config 0 10 1 17) 10
      ]
    , let expected1 =
            [ 123456804, 123456812, 123456820, 123456889, 123456897
            , 123456901, 123456978, 123456986, 123456994, 123457003
            , 123457011, 123457088, 123457096, 123457100, 123457169
            , 123457177, 123457185, 123457193, 123457258, 123457266
            , 123457274, 123457282, 123457290, 123457339, 123457347
            , 123457355, 123457363, 123457371, 123457428, 123457436
            , 123457444, 123457452, 123457460, 123457509, 123457517
            , 123457525, 123457533, 123457541, 123457606, 123457614
            , 123457622, 123457630, 123457699, 123457703, 123457711
            , 123457788]
          expected2 = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
      in
      testGroup "list"
        [ test_list 1 (Config 123456789 123457789 21 1) expected1
        , test_list 5 (Config 123456789 123457789 21 2) expected1
        , test_list 5 (Config 123456789 123457789 21 3) expected1
        , test_list 5 (Config 123456789 123457789 21 8) expected1
        , test_list 1 (Config 0 10 1 3) expected2
        , test_list 1 (Config 0 10 1 17) expected2
        ]
    , testGroup "search"
      [ test_search 1 (Config 274856170 274856190 11 4) "ba9cd915c8e359d9733edcfe9c61e5aca92afb" Nothing
      , test_search 1 (Config 274856170 274856190 12 4) "c736ca9048d0967a27ec3833832f7ffb571ebd2f" Nothing
      , test_search 1 (Config 274856170 274856190 11 4) "c736ca9048d0967a27ec3833832f7ffb571ebd2f" (Just 274856182)
      , test_search 1 (Config 261756170 274896190 47 4) "a8428d9c87323e977978a67ee48827ca154bd84b" (Just 274886190)
      , test_search 1 (Config 261756170 262035744 1 1) "356a192b7913b04c54574d18c28d46e6395428ab" Nothing
      , test_search 5 (Config 261756170 262035744 1 2) "356a192b7913b04c54574d18c28d46e6395428ab" Nothing
      , test_search 5 (Config 261756170 262035744 1 4) "356a192b7913b04c54574d18c28d46e6395428ab" Nothing
      ]
    ]

test_count
    :: Int                  -- test repeats
    -> Config               -- input
    -> Int                  -- expected
    -> TestTree
test_count repeats config expected =
  let test = do
        result <- iban_calculator config Count
        case runParser (sc *> int <* eof) "iban-calculator" result of
          Left err     -> assertFailure (errorBundlePretty err)
          Right actual -> expected @=? actual
  in
  if repeats > 1
    then testGroup (mkName config) $ map (\i -> testCase (show i) test) [1 .. repeats]
    else testCase  (mkName config) test

test_list
    :: Int                  -- repeats
    -> Config               -- input
    -> [Int]                -- expected (sorted)
    -> TestTree
test_list repeats config expected =
  let test = do
        result <- iban_calculator config List
        case runParser (sc *> many pair <* eof) "iban-calculator" result of
          Left err     -> assertFailure (errorBundlePretty err)
          Right actual -> do
            let (seqs, accs) = unzip actual
                n            = length expected
            --
            assertEqual "sequence numbers are incorrect" seqs [1..n]
            assertEqual "bank account numbers are incorrect" expected (sort accs)
  in
  if repeats > 1
    then testGroup (mkName config) $ map (\i -> testCase (show i) test) [1 .. repeats]
    else testCase  (mkName config) test

test_search
    :: Int                  -- repeats
    -> Config               -- input
    -> ByteString           -- target
    -> Maybe Int            -- expected
    -> TestTree
test_search repeats config target mexpected =
  let test = do
        result <- iban_calculator config (Search target)
        case mexpected of
          Nothing       -> case runParser (sc *> symbol "not found" <* eof) "iban-calculator" result of
                             Left err -> assertFailure (errorBundlePretty err)
                             Right _  -> return ()
          Just expected -> case runParser (sc *> int <* eof) "iban-calculator" result of
                             Left err      -> assertFailure (errorBundlePretty err)
                             Right actual  -> expected @=? actual
  in
  if repeats > 1
    then testGroup (mkName config) $ map (\i -> testCase (show i) test) [1 .. repeats]
    else testCase  (mkName config) test


mkName :: Config -> TestName
mkName Config{..} = printf "%d %d %d %d" cfgLower cfgUpper cfgModulus cfgThreads


iban_calculator_path :: FilePath
iban_calculator_path = unsafePerformIO $ do
  path <- readProcess "stack" ["path", "--local-install-root"] []
  return (init path </> "bin" </> "iban-calculator")

iban_calculator :: Config -> Mode -> IO String
iban_calculator Config{..} mode =
  let flags = [ show cfgLower
              , show cfgUpper
              , show cfgModulus
              , show cfgThreads
              ] ++ case mode of
                     Count       -> ["count"]
                     List        -> ["list"]
                     Search hash -> ["search", B8.unpack hash]
  in
  readProcess iban_calculator_path flags []

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 comment empty

comment :: Parser ()
comment = L.skipLineComment "#" >> (void eol <|> eof) >> return ()

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

int :: Parser Int
int = L.signed sc (lexeme L.decimal)

pair :: Parser (Int, Int)
pair = do
  x <- int <* sc
  y <- int <* sc
  return (x,y)

