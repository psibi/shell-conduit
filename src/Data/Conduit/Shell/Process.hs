{-# LANGUAGE FlexibleContexts #-}

-- | Reading from the process.

module Data.Conduit.Shell.Process where

import           Data.Conduit.Shell.Types

import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Loop
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.List (sourceList)
import           Data.Conduit.Process
import           Data.Maybe
import           System.Exit (ExitCode(..))
import           System.IO

-- | Size of buffer used to read from process.
bufSize :: Int
bufSize = 64 * 1024

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
conduitProcess cp = bracketP createp closep $ \(Just cin, Just cout, _, ph) -> do
  end <- repeatLoopT $ do
    -- if process's outputs are available, then yields them.
    repeatLoopT $ do
      b <- liftIO $ hReady' cout
      when (not b) exit
      out <- liftIO $ S.hGetSome cout bufSize
      void $ lift . lift $ yield (Right out)

    -- if process exited, then exit
    end <- liftIO $ getProcessExitCode ph
    when (isJust end) $ exitWith end

    inp <- lift await
    case inp of
      -- if upper stream ended, then exit
      Nothing -> exitWith Nothing
      Just c ->
        case c of
          -- pass along errors to next process
          Left{} -> lift (leftover c)
          -- write stdin into this process
          Right s ->
             liftIO (do S.hPut cin s
                        hFlush cin)

  -- uppstream or process is done.
  -- process rest outputs.
  liftIO $ hClose cin
  repeatLoopT $ do
    out <- liftIO $ S.hGetSome cout bufSize
    when (S.null out) exit
    lift $ yield (Right out)

  ec <- liftIO $ maybe (waitForProcess' ph) return end
  case ec of
    ExitSuccess -> return ()
    ExitFailure i -> lift (monadThrow (ShellExitFailure i))

  where
    createp = createProcess cp
      { std_in  = CreatePipe
      , std_out = CreatePipe
      }

    closep (Just cin, Just cout, _, ph) = do
      hClose cin
      hClose cout
      _ <- waitForProcess' ph
      return ()
    closep _ = error "Data.Conduit.Process.closep: Unhandled case"

    hReady' h =
      hReady h `E.catch` \(E.SomeException _) -> return False
    waitForProcess' ph =
      waitForProcess ph `E.catch` \(E.SomeException _) -> return ExitSuccess
