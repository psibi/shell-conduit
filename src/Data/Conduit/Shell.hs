-- | Shell scripting with Conduit.

module Data.Conduit.Shell
  (run
  ,withRights
  ,redirect
  ,shell
  ,proc
  ,discardChunks
  ,module Data.Conduit.Shell.PATH)
  where

import           Data.Conduit.Shell.PATH
import           Data.Conduit.Shell.Process
import           Data.Conduit.Shell.Types

import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Either

-- | Do something with just the rights.
withRights :: (Monad m)
           => Conduit ByteString m ByteString -> Conduit Chunk m Chunk
withRights f =
  getZipConduit
    (ZipConduit f' *>
     ZipConduit g')
  where f' =
          CL.mapMaybe (either (const Nothing) Just) =$=
          f =$=
          CL.map Right
        g' = CL.filter isLeft

-- | Redirect the given chunk type to the other type.
redirect :: Monad m
         => ChunkType -> Conduit Chunk m Chunk
redirect ty =
  CL.map (\c ->
            case c of
              Left x ->
                case ty of
                  Stderr -> Right x
                  Stdout -> c
              Right x ->
                case ty of
                  Stderr -> c
                  Stdout -> Left x)

-- | Discard any output from the command: make it quiet.
quiet :: (Monad m,MonadIO m) => Conduit Chunk m Chunk -> Conduit Chunk m Chunk
quiet m = m $= discardChunks
