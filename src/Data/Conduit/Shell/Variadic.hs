{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Variadic process calling.

module Data.Conduit.Shell.Variadic
  (ProcessType(..)
  ,variadicProcess)
  where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Conduit.Shell.Process
import           Data.Conduit.Shell.Types
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | A variadic process maker.
variadicProcess :: (ProcessType r)
                => String -> r
variadicProcess name = spr name []

-- | Make the final conduit.
makeProcessLauncher :: (MonadResource m)
                    => String -> [String] -> Conduit Chunk m Chunk
makeProcessLauncher name args = proc name args

class ProcessType t where
    spr :: String -> [String] -> t

-- | The real type should be:
--
-- @ConduitM Chunk Chunk m ()@
--
-- But with this more liberal instance head we catch all cases in the
-- instance resolver, and then apply the equality restrictions later.
--
instance (MonadResource m, c ~ Chunk, c' ~ Chunk, r ~ ()) => ProcessType (ConduitM c c' m r) where
    spr name args = makeProcessLauncher name (reverse args)

-- | Accept strings as arguments.
instance (ProcessType r) => ProcessType (String -> r) where
    spr name args = \a -> spr name (a : args)

-- | Accept strict 'ST.Text' as arguments.
instance (ProcessType r) => ProcessType (ST.Text -> r) where
    spr name args = \a -> spr name (ST.unpack a : args)

-- | Accept lazy 'LT.Text' as arguments.
instance (ProcessType r) => ProcessType (LT.Text -> r) where
    spr name args = \a -> spr name (LT.unpack a : args)

-- | Accept strict 'SB.ByteString' as arguments. Encodes as UTF-8.
instance (ProcessType r) => ProcessType (SB.ByteString -> r) where
    spr name args = \a -> spr name (ST.unpack (ST.decodeUtf8 a) : args)

-- | Accept lazy 'LB.ByteString' as arguments. Encodes as UTF-8.
instance (ProcessType r) => ProcessType (LB.ByteString -> r) where
    spr name args = \a -> spr name (LT.unpack (LT.decodeUtf8 a) : args)
