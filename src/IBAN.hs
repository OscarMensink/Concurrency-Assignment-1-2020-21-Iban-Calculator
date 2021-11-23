--
-- INFOB3CC Concurrency
-- Practical 1: IBAN calculator
--
-- http://www.cs.uu.nl/docs/vakken/b3cc/assessment.html
--
module IBAN (

  Mode(..), Config(..),
  count, list, search

) where

import Control.Concurrent
import Crypto.Hash.SHA1
import Data.IORef
import Data.List                                          ( elemIndex )
import Data.Word
import Data.Maybe                                         ( fromJust )
import System.Environment
import System.IO
import Data.ByteString.Char8                              ( ByteString )
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8


-- -----------------------------------------------------------------------------
-- 0. m-test
-- -----------------------------------------------------------------------------

mtest :: Int -> Int -> Bool
mtest =
  -- Implement the m-test here!
  undefined


-- -----------------------------------------------------------------------------
-- 1. Counting mode (3pt)
-- -----------------------------------------------------------------------------

count :: Config -> IO Int
count config = do
  -- Implement count mode here!
  undefined


-- -----------------------------------------------------------------------------
-- 2. List mode (3pt)
-- -----------------------------------------------------------------------------

list :: Config -> IO ()
list config = do
  -- Implement list mode here!
  undefined


-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------

search :: Config -> ByteString -> IO ()
search config query = do
  -- Implement search mode here!
  undefined


-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------

data Mode = Count | List | Search ByteString
  deriving Show

data Config = Config
  { cfgLower   :: !Int
  , cfgUpper   :: !Int
  , cfgModulus :: !Int
  , cfgThreads :: !Int
  }
  deriving Show

-- Evaluates a term, before continuing with the next IO operation.
--
evaluate :: a -> IO ()
evaluate x = x `seq` return ()

-- Atomic compare-and-swap
--
-- Takes a reference, the expected value and a desired value. Replaces the
-- value in the reference with the desired value, if it contained
-- 'expected'. Returns whether the operation was successful.
--
atomicCAS
    :: IORef Int    -- ^ reference
    -> Int          -- ^ expected value
    -> Int          -- ^ desired value
    -> IO Bool
atomicCAS ref expected desired =
  atomicModifyIORef' ref $ \old ->
    if old == expected
       then (desired, True)
       else (old, False)

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var

-- Checks whether 'value' has the expected hash.
--
checkHash :: ByteString -> String -> Bool
checkHash expected value = expected == hash (B8.pack value)

