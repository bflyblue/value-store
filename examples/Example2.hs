{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}

module Main where

import GHC.Generics
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Serialize
import Data.Serialize.Text ()
import Database.Value

data Queue a = Queue [a]
  deriving (Show, Eq, Generic)

instance Serialize a => Serialize (Queue a)

submit :: Serialize a => PVar (Queue a) -> a -> VTM ()
submit q x = do
    Queue xs <- readPVar q
    writePVar q $ Queue (x : xs)

pop :: Serialize a => PVar (Queue a) -> VTM a
pop q = do
    Queue xs <- readPVar q
    case xs of
      [] -> retry
      (x : xs') -> do
          writePVar q $ Queue xs'
          return x

pop' :: Serialize a => PVar (Queue a) -> VTM a
pop' q = pop q `orElse` (liftIO (putStrLn "pop") >> retry)

main :: IO ()
main = do
    void $ forkIO $
      withStore "dbname=value" $ \c -> do
        q <- atomically c $ newPVar "example2-queue" (Queue [])

        forever $ do
          x <- atomically c (pop' q)
          print (x :: Int)

    withStore "dbname=value" $ \c -> do
      q <- atomically c $ newPVar "example2-queue" (Queue [])

      forM_ [1..10] $ \i -> do
        atomically c $ submit q (i :: Int)
        threadDelay 100000
