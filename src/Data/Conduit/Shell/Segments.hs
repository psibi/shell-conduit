-- | Helpful segment combinators.
module Data.Conduit.Shell.Segments where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Shell.Process
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import UnliftIO (MonadUnliftIO)


-- | Extract the 'String' values from a segment.
strings :: MonadUnliftIO m => Segment m () -> Segment m [String]
strings s = s $| conduit (CB.lines .| CL.map S8.unpack .| CL.consume)

-- | Extract the 'Text' values from a segment.
texts :: MonadUnliftIO m => Segment m () -> Segment m [Text]
texts s = s $| conduit (CB.lines .| CL.map T.decodeUtf8 .| CL.consume)

-- | Ignore any output from a segment.
ignore :: MonadUnliftIO m => Segment m () -> Segment m ()
ignore s = void (s $| conduit CL.consume)
