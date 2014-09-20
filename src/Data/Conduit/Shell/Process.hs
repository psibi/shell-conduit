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
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Loop
import           Control.Monad.Trans.Resource
import           Data.ByteString
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.List (sourceList)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Either
import           Data.Maybe
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
conduitProcess
  :: (MonadResource m)
     => CreateProcess
     -> Conduit Chunk m Chunk
conduitProcess cp =
  bracketP createp
           closep
           (\(Just cin,Just cout,Just cerr,ph) ->
              do let proxy =
                       do b <- liftIO (hReady' cout)
                          when (not b) exit
                          out <- liftIO (S.hGetSome cout bufSize)
                          (void . lift . lift) (yield (Right out))
                 end <- repeatLoopT
                          (do repeatLoopT proxy
                              end <- liftIO (getProcessExitCode ph)
                              when (isJust end)
                                   (exitWith end)
                              inp <- lift await
                              case inp of
                                Nothing ->
                                  exitWith Nothing
                                Just c ->
                                  case c of
                                    Left{} ->
                                      lift (leftover c)
                                    Right s ->
                                      liftIO (do S.hPut cin s
                                                 hFlush cin))
                 liftIO (hClose cin)
                 repeatLoopT
                   (do out <- liftIO (S.hGetSome cout bufSize)
                       when (S.null out) exit
                       lift (yield (Right out)))
                 ec <- liftIO (maybe (waitForProcess' ph) return end)
                 case ec of
                   ExitSuccess -> return ()
                   ExitFailure i ->
                     lift (monadThrow (ShellExitFailure i)))
  where createp =
          createProcess
            cp {std_in = CreatePipe
               ,std_out = CreatePipe
               ,std_err = CreatePipe}
        closep (Just cin,Just cout,_,ph) =
          do hClose cin
             hClose cout
             _ <- waitForProcess' ph
             return ()
        closep _ =
          error "Data.Conduit.Process.closep: Unhandled case"
        hReady' h =
          hReady h `E.catch`
          \(E.SomeException _) -> return False
        waitForProcess' ph =
          waitForProcess ph `E.catch`
          \(E.SomeException _) ->
            return ExitSuccess
