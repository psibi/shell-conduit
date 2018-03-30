{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

-- | All types.
module Data.Conduit.Shell.Types where

import Control.Applicative
import UnliftIO.Exception
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Typeable

-- | Shell transformer.
newtype ShellT m a = ShellT
  { runShellT :: ResourceT m a
  } deriving (Applicative, Monad, Functor, MonadThrow, MonadIO, MonadTrans)

deriving instance (MonadUnliftIO m) => MonadResource (ShellT m)

-- | Intentionally only handles 'ShellException'. Use normal exception
-- handling to handle usual exceptions.
instance (MonadUnliftIO (ShellT m), Applicative m, MonadThrow m) =>
         Alternative (ConduitT i o (ShellT m)) where
  empty = throwIO ShellEmpty
  x <|> y = do
    r <- tryC x
    case r of
      Left (_ :: ShellException) -> y
      Right rr -> return rr

-- | An exception resulting from a shell command.
data ShellException
  = ShellEmpty -- ^ For 'mempty'.
  | ShellExitFailure !Int -- ^ Process exited with failure.
  deriving (Typeable, Show)

instance Exception ShellException
