{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Reading from the process.

module Data.Conduit.Shell.Process
  (-- * Running scripts
   run
   -- * Conduit types
  ,text
  ,bytes
  -- * General conduits
  ,conduit
  ,conduitEither
  -- * Running processes
  ,Data.Conduit.Shell.Process.shell
  ,Data.Conduit.Shell.Process.proc
  ,($|)
  ,Segment
  ,ProcessException(..)
  ,ToChunk(..)
  ,tryS
  )
  where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Conduit (MonadThrow)
import           Data.Conduit.Text (encodeUtf8, decodeUtf8)
import           Data.Text (Text)
import           Data.Typeable
import           System.Exit
import           System.IO
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process hiding (createPipe)
import           UnliftIO (MonadUnliftIO, unliftIO, askUnliftIO)

-- | A pipeable segment. Either a conduit or a process.
data Segment m r
  = SegmentConduit (ConduitM ByteString (Either ByteString ByteString) m r)
  | SegmentProcess (Handles -> m r)

instance MonadIO m => Monad (Segment m) where
  return = SegmentConduit . return
  SegmentConduit c >>= f =
    SegmentProcess (conduitToProcess c) >>=
    f
  SegmentProcess f >>= g =
    SegmentProcess
      (\handles ->
         do x <- f handles
            case g x of
              SegmentConduit c ->
                conduitToProcess c handles
              SegmentProcess p -> p handles)

instance MonadIO m => Functor (Segment m) where
  fmap = liftM

instance MonadIO m => Applicative (Segment m) where
  (<*>) = ap; pure = return

instance MonadUnliftIO m => Alternative (Segment m) where
  this <|> that =
    do ex <- tryS this
       case ex of
         Right x -> pure x
         Left (_ :: ProcessException) -> that
  empty = throw ProcessEmpty

-- | Try something in a segment.
tryS :: (Exception e, MonadUnliftIO m) => Segment m r -> Segment m (Either e r)
tryS s =
  case s of
    SegmentConduit c -> SegmentConduit (tryC c)
    SegmentProcess f -> SegmentProcess $ (\h -> do
                                             u <- askUnliftIO
                                             liftIO $ try $ unliftIO u (f h))

instance MonadIO m => MonadIO (Segment m) where
  liftIO x = SegmentProcess (const $ liftIO  x)

-- | Process handles: @stdin@, @stdout@, @stderr@
data Handles =
  Handles Handle
          Handle
          Handle

-- | Process running exception.
data ProcessException
  = ProcessException CreateProcess
                     ExitCode
  | ProcessEmpty
  deriving (Typeable)

instance Exception ProcessException

instance Show ProcessException where
  show ProcessEmpty = "empty process"
  show (ProcessException cp ec) =
    concat
      [ "The "
      , case cmdspec cp of
          ShellCommand s -> "shell command " ++ show s
          RawCommand f args -> "raw command: " ++ unwords (f : map show args)
      , " returned a failure exit code: "
      , case ec of
          ExitFailure i -> show i
          _ -> show ec
      ]

-- | Convert a process or a conduit to a segment.
class ToSegment m a  where
  type SegmentResult m a
  toSegment :: a -> Segment m (SegmentResult m a)

instance ToSegment m (Segment m r) where
  type SegmentResult m (Segment m r) = r
  toSegment = id

instance (a ~ ByteString, ToChunk b, Monad m) =>
         ToSegment m (ConduitT a b m r) where
  type SegmentResult m (ConduitT a b m r) = r
  toSegment f = SegmentConduit (f `fuseUpstream` CL.map toChunk)

instance MonadIO m => ToSegment m CreateProcess where
  type SegmentResult m CreateProcess = ()
  toSegment = liftProcess

-- | Used to allow outputting stdout or stderr.
class ToChunk a  where
  toChunk :: a -> Either ByteString ByteString

instance ToChunk ByteString where
  toChunk = Left

instance ToChunk (Either ByteString ByteString) where
  toChunk = id

-- | Run a shell command.
shell :: MonadIO m => String -> Segment m ()
shell = liftProcess . System.Process.shell

-- | Run a process command.
proc :: MonadIO m => String -> [String] -> Segment m ()
proc name args = liftProcess (System.Process.proc name args)

-- | Run a segment.
run :: MonadIO m => Segment m r -> m r
run (SegmentConduit c) = run (SegmentProcess (conduitToProcess c))
run (SegmentProcess p) = p (Handles stdin stdout stderr)

-- | Fuse two segments (either processes or conduits).
($|) :: MonadUnliftIO m => Segment m () -> Segment m b -> Segment m b
x $| y = x `fuseSegment` y

infixl 0 $|

-- | Work on the stream as 'Text' values from UTF-8.
text
  :: (r ~ (), MonadThrow m)
  => ConduitT Text Text m r -> Segment m r
text conduit' = bytes (decodeUtf8 .| conduit' .| encodeUtf8)

-- | Lift a conduit into a segment.
bytes
  :: (a ~ ByteString, Monad m)
  => ConduitT a ByteString m r -> Segment m r
bytes f = SegmentConduit (f `fuseUpstream` CL.map toChunk)

-- | Lift a conduit into a segment.
conduit
  :: (a ~ ByteString, Monad m)
  => ConduitT a ByteString m r -> Segment m r
conduit f = SegmentConduit (f `fuseUpstream` CL.map toChunk)

-- | Lift a conduit into a segment, which can yield stderr.
conduitEither
  :: (a ~ ByteString, Monad m)
  => ConduitT a (Either ByteString ByteString) m r -> Segment m r
conduitEither f = SegmentConduit (f `fuseUpstream` CL.map toChunk)

-- | Lift a process into a segment.
liftProcess :: MonadIO m => CreateProcess -> Segment m ()
liftProcess cp =
  SegmentProcess
    (\(Handles inh outh errh) ->
        let config =
              cp
              { std_in = UseHandle inh
              , std_out = UseHandle outh
              , std_err = UseHandle errh
              , close_fds = True
              }
        in
          liftIO $ do
              (Nothing, Nothing, Nothing, ph) <- createProcess_ "liftProcess" config
              ec <- waitForProcess ph
              case ec of
                ExitSuccess -> return ()
                _ -> throwIO (ProcessException cp ec))

-- | Convert a conduit to a process.
conduitToProcess :: MonadIO m => ConduitT ByteString (Either ByteString ByteString) m r
                 -> (Handles -> m r)
conduitToProcess c (Handles inh outh errh) =
  runConduit $ sourceHandle inh .| c `fuseUpstream` sinkHandles outh errh

-- | Sink everything into the two handles.
sinkHandles ::
            MonadIO m
            => Handle
            -> Handle
            -> ConduitT (Either ByteString ByteString) Void m ()
sinkHandles out err =
  CL.mapM_
    (\ebs ->
        liftIO $ case ebs of
          Left bs -> S.hPut out bs
          Right bs -> S.hPut err bs)

-- | Create a pipe.
createHandles :: IO (Handle, Handle)
createHandles =
  mask_
    (do (inFD, outFD) <- createPipe
        x <- fdToHandle inFD
        y <- fdToHandle outFD
        hSetBuffering x NoBuffering
        hSetBuffering y NoBuffering
        return (x, y))

-- | Fuse two processes.
fuseProcess :: MonadUnliftIO m => (Handles -> m ()) -> (Handles -> m r) -> (Handles -> m r)
fuseProcess left right (Handles in1 out2 err) = do
  u <- askUnliftIO
  (in2, out1) <- liftIO createHandles
  liftIO $ runConcurrently
    (Concurrently ((unliftIO u $ left (Handles in1 out1 err)) `finally` hClose out1) *>
     Concurrently ((unliftIO u $ right (Handles in2 out2 err)) `finally` hClose in2))

-- | Fuse two conduits.
fuseConduit
  :: Monad m
  => ConduitT ByteString (Either ByteString ByteString) m ()
  -> ConduitT ByteString (Either ByteString ByteString) m r
  -> ConduitT ByteString (Either ByteString ByteString) m r
fuseConduit left right = left .| getZipConduit right'
  where
    right' =
      ZipConduit (CL.filter isRight) *>
      ZipConduit (CL.mapMaybe (either (const Nothing) Just) .| right)
    isRight Right {} = True
    isRight Left {} = False

-- | Fuse a conduit with a process.
fuseConduitProcess
  :: MonadUnliftIO m
  => ConduitT ByteString (Either ByteString ByteString) m ()
  -> (Handles -> m r)
  -> (Handles -> m r)
fuseConduitProcess left right (Handles in1 out2 err) = do
  u <- askUnliftIO
  (in2, out1) <- liftIO createHandles
  liftIO $ runConcurrently
    (Concurrently
       ((unliftIO u $ runConduit $ sourceHandle in1 .| left .| sinkHandles out1 err) `finally`
        hClose out1) *>
     Concurrently ((unliftIO u $ right (Handles in2 out2 err)) `finally` hClose in2))

-- | Fuse a process with a conduit.
fuseProcessConduit
  :: MonadUnliftIO m
  => (Handles -> m ())
  -> ConduitT ByteString (Either ByteString ByteString) m r
  -> (Handles -> m r)
fuseProcessConduit left right (Handles in1 out2 err) = do
  u <- askUnliftIO
  (in2, out1) <- liftIO createHandles
  liftIO $ runConcurrently
    (Concurrently ((unliftIO u $ left (Handles in1 out1 err)) `finally` hClose out1) *>
     Concurrently
       ((unliftIO u $ runConduit $
         sourceHandle in2 .| right `fuseUpstream` sinkHandles out2 err) `finally`
        hClose in2))

-- | Fuse one segment with another.
fuseSegment :: MonadUnliftIO m => Segment m () -> Segment m r -> Segment m r
SegmentConduit x `fuseSegment` SegmentConduit y =
  SegmentConduit (fuseConduit x y)
SegmentConduit x `fuseSegment` SegmentProcess y =
  SegmentProcess (fuseConduitProcess x y)
SegmentProcess x `fuseSegment` SegmentConduit y =
  SegmentProcess (fuseProcessConduit x y)
SegmentProcess x `fuseSegment` SegmentProcess y =
  SegmentProcess (fuseProcess x y)
