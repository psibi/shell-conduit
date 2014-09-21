{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Reading from the process.

module Data.Conduit.Shell.Process
  (-- * Running scripts
   run
   -- * Running processes
  ,Data.Conduit.Shell.Process.shell
  ,Data.Conduit.Shell.Process.proc
   -- * I/O chunks
  ,withRights
  ,redirect
  ,quiet
  ,writeChunks
  ,discardChunks
  -- * Low-level internals
  ,conduitProcess
  )
  where

import           Data.Conduit.Shell.Types

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.List (sourceList)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Semigroup
import           Data.These
import           System.Exit (ExitCode(..))
import           System.IO
import qualified System.Process

-- | Run a shell command.
shell :: (MonadResource m) => String -> Conduit Chunk m Chunk
shell = conduitProcess . System.Process.shell

-- | Run a shell command.
proc :: (MonadResource m) => String -> [String] -> Conduit Chunk m Chunk
proc px args = conduitProcess (System.Process.proc px args)

-- | Size of buffer used to read from process.
bufSize :: Int
bufSize = 64 * 1024

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
  CL.map (\c' ->
            case c' of
              Left x' ->
                case ty of
                  Stderr -> Right x'
                  Stdout -> c'
              Right x' ->
                case ty of
                  Stderr -> c'
                  Stdout -> Left x')

-- | Discard any output from the command: make it quiet.
quiet :: (Monad m,MonadIO m) => Conduit Chunk m Chunk -> Conduit Chunk m Chunk
quiet m = m $= discardChunks

-- | Run a shell scripting conduit.
run :: (MonadIO m,MonadBaseControl IO m)
    => Conduit Chunk (ShellT m) Chunk -> m ()
run p =
  runResourceT
    (runShellT (sourceList [] $=
                p $$
                writeChunks))

-- | Write chunks to stdout and stderr.
writeChunks :: (MonadIO m)
            => Consumer Chunk m ()
writeChunks =
  awaitForever
    (\c ->
       case c of
         Left e -> liftIO (S.hPut stderr e)
         Right o -> liftIO (S.hPut stdout o))

-- | Discard all chunks.
discardChunks :: (MonadIO m)
              => Consumer Chunk m ()
discardChunks = awaitForever (const (return ()))

-- | Conduit of process.
conduitProcess :: (MonadResource m)
               => CreateProcess -> Conduit Chunk m Chunk
conduitProcess cp =
  bracketP createp closep startProxy
  where createp =
          createProcess
            cp {std_in = CreatePipe
               ,std_out = CreatePipe
               ,std_err = CreatePipe}
        closep (Just cin,Just cout,Just cerr,ph) =
          do hClose cin
             hClose cout
             hClose cerr
             _ <- waitForProcess' ph
             return ()
        closep _ = error "conduitProcess: unexpected arguments to closep"

-- | Start proxying from conduit to process back to conduit.
startProxy :: (MonadIO m,MonadThrow m)
           => (Maybe Handle,Maybe Handle,Maybe Handle,ProcessHandle)
           -> ConduitM Chunk Chunk m ()
startProxy (Just cin,Just cout,Just cerr,ph) = interleave
  where interleave =
          do end <- proxyInterleaved
             liftIO (hClose cin)
             remainders cout cerr
             ec <- liftIO (maybe (waitForProcess' ph) return end)
             case ec of
               ExitSuccess -> return ()
               ExitFailure i ->
                 monadThrow (ShellExitFailure i)
        proxyInterleaved =
          do proxy cout Right
             proxy cerr Left
             ended <- liftIO (getProcessExitCode ph)
             case ended of
               Just{} -> return ended
               Nothing ->
                 do minp <- await
                    case minp of
                      Nothing -> return Nothing
                      Just chunk ->
                        do case chunk of
                             Left{} -> yield chunk
                             Right bytes ->
                               liftIO (do S.hPut cin bytes
                                          hFlush cin)
                           proxyInterleaved
startProxy _ = error "startProxy: unexpected arguments"

remainders cout cerr =
  do chan <- liftIO newChan
     void (liftIO (forkIO (remainder "stdout" chan cout Right)))
     void (liftIO (forkIO (remainder "stderr" chan cerr Left)))
     fix (\loop done ->
            case done of
              Just (These () ()) -> return ()
              _ ->
                do chunk <- liftIO (readChan chan)
                   case chunk of
                     Left mchunk ->
                       case mchunk of
                         Nothing ->
                           loop (done <>
                                 Just (This ()))
                         Just chunk ->
                           do yield (Left chunk)
                              loop done
                     Right mchunk ->
                       case mchunk of
                         Nothing ->
                           loop (done <>
                                 Just (That ()))
                         Just chunk ->
                           do yield (Right chunk)
                              loop done)
         (Nothing :: Maybe (These () ()))

-- | Proxy final results from the handle and yield them.
remainder :: String
          -> Chan (Either (Maybe ByteString) (Maybe ByteString))
          -> Handle
          -> (Maybe ByteString -> Either (Maybe ByteString) (Maybe ByteString))
          -> IO ()
remainder l chan h cons =
  do bytes <- S.hGetSome h bufSize
     if S.null bytes
        then writeChan chan (cons Nothing)
        else do writeChan chan (cons (Just bytes))
                remainder l chan h cons

-- | Proxy live results from the given handle and yield them.
proxy :: MonadIO m
      => Handle -> (ByteString -> o) -> ConduitM i o m ()
proxy h cons =
  do ready <- liftIO (hReady' h)
     if not ready
        then return ()
        else do bytes <- liftIO (S.hGetSome h bufSize)
                yield (cons bytes)
                proxy h cons

-- | Is the handle ready? Catches any exceptions.
hReady' :: Handle -> IO Bool
hReady' h =
  E.catch (hReady h)
          (\(E.SomeException _) -> return False)

-- | A safer 'waitForProcess'.
waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' ph =
  E.catch (waitForProcess ph)
          (\(E.SomeException _) ->
             return ExitSuccess)

-- | Polyfill for base < 4.7
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
