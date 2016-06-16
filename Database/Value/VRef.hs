{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}

module Database.Value.VRef
  ( VRef
  , persist
  , persistMaybe
  , deref
  , derefMaybe
  , mapRef
  )
where

import Prelude hiding (lookup)

import GHC.Generics

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.RWS.Strict as RWS

import Crypto.Hash

import Data.ByteString
import Data.Serialize (Serialize, encode, decode)
import Data.Typeable

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.SqlQQ

import Database.Value.VTM

data VRef a = VRef ByteString deriving (Show, Eq, Generic, Typeable)

instance Serialize (VRef a)

instance ToField (VRef a) where
    toField (VRef ref) = toField ref

instance FromField (VRef a) where
    fromField f meta = VRef <$> fromField f meta

hashval :: Serialize a => a -> Digest SHA256
hashval = hash . encode

hashstr :: Serialize a => a -> ByteString
hashstr = digestToHexByteString . hashval

persist :: Serialize a => a -> VTM (VRef a)
persist val = do
    conn <- RWS.ask
    let hstr = hashstr val
    liftIO $ do
        void $ execute conn [sql| DELETE FROM value WHERE hashstr = ? |] (Only hstr)
        void $ execute conn [sql| INSERT INTO value (hashstr, value) VALUES (?, ?) |] (hstr, encode val)
    return (VRef hstr)

deref :: Serialize a => VRef a -> VTM a
deref ref = do
    conn <- RWS.ask
    vs <- liftIO $ query conn [sql| SELECT value FROM value WHERE hashstr = ? |] (Only ref)
    case vs of
        [Only (Binary bs)] -> case decode bs of
                                  Left err  -> error $ "Error decoding VRef `" ++ show ref ++ "': " ++ err
                                  Right val -> return val
        _                  -> error $ "Error reading VRef `" ++ show ref ++ "'"

derefMaybe :: Serialize a => Maybe (VRef a) -> VTM (Maybe a)
derefMaybe (Just ref) = Just <$> deref ref
derefMaybe Nothing    = return Nothing

persistMaybe :: Serialize a => Maybe a -> VTM (Maybe (VRef a))
persistMaybe (Just x) = Just <$> persist x
persistMaybe Nothing  = return Nothing

mapRef :: (Serialize a, Serialize b) => (a -> VTM b) -> VRef a -> VTM (VRef b)
mapRef f = deref >=> f >=> persist
