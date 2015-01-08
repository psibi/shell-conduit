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
import Control.Exception
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Typeable

-- | Shell transformer.
newtype ShellT m a =
  ShellT {runShellT :: ResourceT m a}
  deriving (Applicative,Monad,Functor,MonadThrow,MonadIO,MonadTrans)

deriving instance (MonadResourceBase m) => MonadBase IO (ShellT m)
deriving instance (MonadResourceBase m) => MonadResource (ShellT m)

#if MIN_VERSION_monad_control(1,0,0)
newtype StMShell m a = StMShell{unStMGeoServer :: StM (ResourceT m) a}
#endif

-- | Dumb instance.
instance (MonadThrow m,MonadIO m,MonadBaseControl IO m) => MonadBaseControl IO (ShellT m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (ShellT m) a = StMShell m a
#else
  newtype StM (ShellT m) a = StMShell{unStMGeoServer ::
                                    StM (ResourceT m) a}
#endif
  liftBaseWith f =
    ShellT (liftBaseWith
              (\run ->
                 f (liftM StMShell .
                    run .
                    runShellT)))
  restoreM = ShellT . restoreM . unStMGeoServer

-- | Intentionally only handles 'ShellException'. Use normal exception
-- handling to handle usual exceptions.
instance (MonadBaseControl IO (ShellT m),Applicative m,MonadThrow m) => Alternative (ConduitM i o (ShellT m)) where
  empty = monadThrow ShellEmpty
  x <|> y =
    do r <- tryC x
       case r of
         Left (_ :: ShellException) -> y
         Right rr -> return rr

-- | An exception resulting from a shell command.
data ShellException
  = ShellEmpty -- ^ For 'mempty'.
  | ShellExitFailure !Int -- ^ Process exited with failure.
  deriving (Typeable,Show)
instance Exception ShellException
