{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}

module Main where

import GHC.Generics
import Data.Serialize
import Data.Serialize.Text ()
import Data.Text
import Data.Typeable
import Database.PostgreSQL.Simple
import Database.Value

data Account = Account
  { accountNumber   :: Text
  , accountBalance  :: Integer
  }
  deriving (Show, Eq, Generic, Typeable)

instance Serialize Account

transfer :: PVar Account -> PVar Account -> Integer -> VTM ()
transfer from_ to_ amount = do
    Account num1 bal1 <- readPVar from_
    Account num2 bal2 <- readPVar to_
    writePVar from_ $ Account num1 (bal1 - amount)
    writePVar to_   $ Account num2 (bal2 + amount)

showAccount :: Connection -> PVar Account -> IO ()
showAccount c a = atomically c (readPVar a) >>= print

main :: IO ()
main = withStore "dbname=value" $ \c -> do
    a1234 <- atomically c $ newPVar "account-1234" (Account "1234" 1000)
    a4321 <- atomically c $ newPVar "account-4321" (Account "4321" 1000)

    putStrLn "Before:"
    showAccount c a1234
    showAccount c a4321

    atomically c $
        transfer a1234 a4321 200

    putStrLn "After:"
    showAccount c a1234
    showAccount c a4321
