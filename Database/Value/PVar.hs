{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}

module Database.Value.PVar
  ( PVar
  , newPVar
  , readPVar
  , writePVar
  )
where

import GHC.Generics

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.RWS.Strict as RWS

import Data.Serialize (Serialize, encode, decode)
import Data.Set as Set
import Data.Typeable

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Database.Value.VTM

data PVar a = PVar Label deriving (Show, Eq, Generic, Typeable)

newPVar :: Serialize a => Label -> a -> VTM (PVar a)
newPVar label val = do
    conn <- RWS.ask
    liftIO $ withSavepoint conn (newPVar' conn) `catch` pgerror
    return (PVar label)
  where
    newPVar' c =
        void $ execute c [sql| INSERT INTO variable (label, value) SELECT ?, ? WHERE NOT EXISTS
                              (SELECT 1 FROM variable WHERE label = ?) |] (label, Binary (encode val), label)
    -- Ignore unique constraint if variable exists
    pgerror (SqlError "23505" _ _ _ _) = return ()
    pgerror e                          = throw e

readPVar :: Serialize a => PVar a -> VTM a
readPVar (PVar label) = do
    conn <- RWS.ask
    RWS.modify $ Set.insert label
    vs <- liftIO $ query conn [sql| SELECT value FROM variable WHERE label = ? |] (Only label)
    case vs of
        [Only (Binary bs)] -> case decode bs of
                                  Left err  -> error $ "Error decoding PVar `" ++ show label ++ "': " ++ err
                                  Right val -> return val
        _                  -> error $ "Error reading PVar `" ++ show label ++ "'"

writePVar :: Serialize a => PVar a -> a -> VTM ()
writePVar (PVar label) val = do
    conn <- RWS.ask
    liftIO $ do
      void $ execute conn [sql| DELETE FROM variable WHERE label = ? |] (Only label)
      void $ execute conn [sql| INSERT INTO variable (label, value) VALUES (?, ?) |] (label, Binary (encode val))
      void $ execute conn [sql| NOTIFY var, ? |] (Only label)
