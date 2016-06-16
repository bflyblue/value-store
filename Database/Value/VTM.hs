{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Database.Value.VTM
  ( VTM
  , Label
  , atomically
  , retry
  , orElse
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.RWS.Strict as RWS

import Data.ByteString
import Data.Monoid
import Data.Set as Set
import Data.Text
import Data.Text.Encoding
import Data.Typeable

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Transaction

type VTM = RWS.RWST Connection () (Set Label) IO

type Label = Text

data VTMException = Retry (Set ByteString) deriving (Show, Typeable)
instance Exception VTMException

atomically :: Connection -> VTM a -> IO a
atomically conn x = do
    r <- withTransactionSerializable conn (Right . fst <$> RWS.evalRWST x conn Set.empty) `catch` vtmerror
    case r of
        Right a   -> return a
        Left vars -> wait vars >> atomically conn x
  where
    vtmerror (Retry vars) = return (Left vars)

    wait watched =
        unless (Set.null watched) $ do
            void $ execute_ conn "LISTEN var"
            n <- getNotification conn
            void $ execute_ conn "UNLISTEN var"
            unless (notificationChannel n == "var" && notificationData n `Set.member` watched)
                (wait watched)

retry :: VTM a
retry = do
    vars <- RWS.get
    throw $ Retry (Set.map encodeUtf8 vars)

orElse :: VTM a -> VTM a -> VTM a
orElse a b = do
    conn <- RWS.ask
    liftIO $ tryA conn
  where
    tryA c =
        (fst <$> withSavepoint c (RWS.evalRWST a c Set.empty)) `catch` tryB c

    tryB c (Retry varsA) =
        (fst <$> withSavepoint c (RWS.evalRWST b c Set.empty)) `catch` failboth varsA

    failboth varsA (Retry varsB) = throw $ Retry (varsA <> varsB)
