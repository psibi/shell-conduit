{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Variadic process calling.

module Data.Conduit.Shell.Variadic
  (ProcessType(..)
  ,variadicProcess
  ,CmdArg(..))
  where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit.Shell.Process
import qualified Data.Text as ST
import qualified Data.Text.Encoding as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | A variadic process maker.
variadicProcess :: (ProcessType r)
                => String -> r
variadicProcess name = spr name []

-- | Make the final conduit.
makeProcessLauncher :: String -> [ST.Text] -> Segment ()
makeProcessLauncher name args = proc name (map ST.unpack args)

-- | Process return type.
class ProcessType t where
    spr :: String -> [ST.Text] -> t

instance (r ~ ()) => ProcessType (Segment r) where
    spr name args = makeProcessLauncher name (reverse args)

-- | Accept strings as arguments.
instance (ProcessType r, CmdArgs a) => ProcessType (a -> r) where
    spr name args = \a -> spr name (toTextArgs a ++ args)

-- | Command line argument.
class CmdArg a where
  toTextArg :: a -> ST.Text

instance CmdArg ST.Text where toTextArg = id
instance CmdArg LT.Text where toTextArg = LT.toStrict
instance CmdArg SB.ByteString where toTextArg = ST.decodeUtf8
instance CmdArg LB.ByteString where toTextArg = LT.toStrict . LT.decodeUtf8
instance CmdArg String where toTextArg = ST.pack

class CmdArgs a where
  toTextArgs :: a -> [ ST.Text ]

instance CmdArgs ST.Text where toTextArgs = return . toTextArg
instance CmdArgs LT.Text where toTextArgs = return . toTextArg
instance CmdArgs SB.ByteString where toTextArgs = return . toTextArg
instance CmdArgs LB.ByteString where toTextArgs = return . toTextArg
instance CmdArgs String where toTextArgs = return . toTextArg

instance (CmdArg a) => CmdArgs [a] where
  toTextArgs = reverse . map toTextArg
