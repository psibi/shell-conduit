-- | All types.

module Data.Conduit.Shell.Types where

import Data.ByteString (ByteString)

-- | A chunk, either stdout/stdin or stderr. Used for both input and
-- output.
type Chunk = Either ByteString ByteString

-- | Either stdout or stderr.
data ChunkType
  = Stderr
  | Stdout
  deriving (Eq,Enum,Bounded)
