module Database.Value
  ( withStore

  , VTM
  , Label
  , atomically
  , retry
  , orElse

  , PVar
  , newPVar
  , readPVar
  , writePVar

  , VRef
  , persist
  , persistMaybe
  , deref
  , derefMaybe
  , mapRef
  )
where

import Database.Value.VTM
import Database.Value.PVar
import Database.Value.VRef

import Control.Exception
import Data.ByteString
import Database.PostgreSQL.Simple

withStore :: ByteString -> (Connection -> IO a) -> IO a
withStore connstr = bracket (connectPostgreSQL connstr) close
