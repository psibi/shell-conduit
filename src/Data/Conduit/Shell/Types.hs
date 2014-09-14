-- | All types.

module Data.Conduit.Shell.Types where

import Data.ByteString (ByteString)

-- | A chunk, either stdout/stdin or stderr. Used for both input and
-- output.
data Chunk
  = StdErr !ByteString
  | StdInOut !ByteString

-- | Either stdout or stderr.
data ChunkType
  = Stderr
  | Stdout
  deriving (Eq,Enum,Bounded)
