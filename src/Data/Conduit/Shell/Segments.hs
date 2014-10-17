-- | Helpful segment combinators.

module Data.Conduit.Shell.Segments where

import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Shell.Process
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Extract the 'String' values from a segment.
strings :: Segment () -> Segment [String]
strings s = s $| conduit (CB.lines $= CL.map S8.unpack $= CL.consume)

-- | Extract the 'Text' values from a segment.
texts :: Segment () -> Segment [Text]
texts s = s $| conduit (CB.lines $= CL.map T.decodeUtf8 $= CL.consume)

-- | Ignore any output from a segment.
ignore :: Segment () -> Segment ()
ignore s = fmap (const ()) (s $| conduit (CL.consume))
