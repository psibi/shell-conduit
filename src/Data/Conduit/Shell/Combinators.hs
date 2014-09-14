-- | Combinators for shell scripting.

module Data.Conduit.Shell.Combinators where

import           Data.Conduit.Shell.Types

import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL

-- | Map a function over stdout.
map :: Monad m
    => (ByteString -> ByteString) -> Conduit Chunk m Chunk
map f =
  CL.map (\c ->
            case c of
              StdInOut x -> StdInOut (f x)
              _ -> c)
