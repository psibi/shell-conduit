{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Reading from the process.

module Data.Conduit.Shell.Process
  (-- * Running scripts
   run
   -- * Running processes
  ,liftConduit
  ,liftProcess
  ,Data.Conduit.Shell.Process.shell
  ,Data.Conduit.Shell.Process.proc
  ,($|)
  ,Segment
  ,ProcessException(..)
  ,ToChunk(..)
  )
  where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Shell.Types
import           Data.Typeable
import           System.Exit
import           System.IO
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process
import           System.Process.Internals (createProcess_)

-- | A pipeable segment. Either a conduit or a process.
data Segment r
  = SegmentConduit (ConduitM ByteString (Either ByteString ByteString) IO r)
  | SegmentProcess (Handles -> IO r)

instance Monad Segment where
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

instance MonadIO Segment where
  liftIO x = SegmentProcess (const x)

-- | Process handles: @stdin@, @stdout@, @stderr@
data Handles =
  Handles {hin :: Handle -- ^ stdin
          ,hout :: Handle -- ^ stdout
          ,herr :: Handle -- ^ stderr
          }

-- | Process running exception.
data ProcessException =
  ProcessException CreateProcess
                   ExitCode
  deriving (Typeable)

instance Exception ProcessException

instance Show ProcessException where
  show (ProcessException cp ec) =
    concat ["The "
           ,case cmdspec cp of
              ShellCommand s -> "shell command " ++ show s
              RawCommand f args ->
                "raw command: " ++
                show (f : args)
           ," returned a failure exit code: "
           ,case ec of
              ExitFailure i -> show i
              _ -> show ec]

-- | Convert a process or a conduit to a segment.
class ToSegment a where
  type SegmentResult a
  toSegment :: a -> Segment (SegmentResult a)

instance ToSegment (Segment r) where
  type SegmentResult (Segment r) = r
  toSegment = id

instance (a ~ ByteString,ToChunk b,m ~ IO) => ToSegment (ConduitM a b m r) where
  type SegmentResult (ConduitM a b m r) = r
  toSegment f =
    SegmentConduit (f `fuseUpstream` CL.map toChunk)

instance ToSegment CreateProcess where
  type SegmentResult CreateProcess = ()
  toSegment = liftProcess

-- | Used to allow outputting stdout or stderr.
class ToChunk a where
  toChunk :: a -> Either ByteString ByteString

instance ToChunk ByteString where
  toChunk = Left

instance ToChunk (Either ByteString ByteString) where
  toChunk = id

-- | Run a shell command.
shell :: String -> Segment ()
shell = liftProcess . System.Process.shell

-- | Run a process command.
proc :: String -> [String] -> Segment ()
proc name args = liftProcess (System.Process.proc name args)

-- | Run a segment.
run :: Segment r -> IO r
run (SegmentConduit c) =
  run (SegmentProcess (conduitToProcess c))
run (SegmentProcess p) =
  p (Handles stdin stdout stderr)

-- | Fuse two segments (either processes or conduits).
($|) :: Segment () -> Segment b -> Segment b
x $| y = x `fuseSegment` y
infixl 0 $|

-- | Lift a conduit into a segment.
liftConduit :: (a ~ ByteString,ToChunk b,m ~ IO) => ConduitM a b m r -> Segment r
liftConduit f = SegmentConduit (f `fuseUpstream` CL.map toChunk)

-- | Lift a process into a segment.
liftProcess :: CreateProcess -> Segment ()
liftProcess cp =
  SegmentProcess
    (\(Handles inh outh errh) ->
       let config =
             cp {std_in = UseHandle inh
                ,std_out = UseHandle outh
                ,std_err = UseHandle errh
                ,close_fds = True}
       in do (Nothing,Nothing,Nothing,ph) <- createProcess_ "liftProcess" config
             ec <- waitForProcess ph
             case ec of
               ExitSuccess -> return ()
               _ ->
                 throwIO (ProcessException cp ec))

-- | Convert a conduit to a process.
conduitToProcess :: ConduitM ByteString (Either ByteString ByteString) IO r
                 -> (Handles -> IO r)
conduitToProcess c (Handles inh outh errh) =
  sourceHandle inh $$ c `fuseUpstream`
  sinkHandles outh errh

-- | Sink everything into the two handles.
sinkHandles :: Handle -> Handle -> Consumer (Either ByteString ByteString) IO ()
sinkHandles out err =
  CL.mapM_ (\ebs ->
              case ebs of
                Left bs -> S.hPut out bs
                Right bs -> S.hPut err bs)

-- | Create a pipe.
createHandles :: IO (Handle, Handle)
createHandles =
  mask_ (do (inFD,outFD) <- createPipe
            x <- fdToHandle inFD
            y <- fdToHandle outFD
            hSetBuffering x NoBuffering
            hSetBuffering y NoBuffering
            return (x,y))

-- | Fuse two processes.
fuseProcess :: (Handles -> IO ()) -> (Handles -> IO r) -> (Handles -> IO r)
fuseProcess left right (Handles in1 out2 err) =
  do (in2,out1) <- createHandles
     runConcurrently
       (Concurrently
          (left (Handles in1 out1 err) `finally`
           hClose out1) *>
        Concurrently
          (right (Handles in2 out2 err) `finally`
           hClose in2))

-- | Fuse two conduits.
fuseConduit :: Monad m
            => ConduitM ByteString (Either ByteString ByteString) m ()
            -> ConduitM ByteString (Either ByteString ByteString) m r
            -> ConduitM ByteString (Either ByteString ByteString) m r
fuseConduit left right = left =$= getZipConduit right'
  where right' =
          ZipConduit (CL.filter isRight) *>
          ZipConduit
            (CL.mapMaybe (either (const Nothing) Just) =$=
             right)
        isRight Right{} = True
        isRight Left{} = False

-- | Fuse a conduit with a process.
fuseConduitProcess :: ConduitM ByteString (Either ByteString ByteString) IO ()
                   -> (Handles -> IO r)
                   -> (Handles -> IO r)
fuseConduitProcess left right (Handles in1 out2 err) =
  do (in2,out1) <- createHandles
     runConcurrently
       (Concurrently
          ((sourceHandle in1 $$ left =$
            sinkHandles out1 err) `finally`
           hClose out1) *>
        Concurrently
          (right (Handles in2 out2 err) `finally`
           hClose in2))

-- | Fuse a process with a conduit.
fuseProcessConduit :: (Handles -> IO ())
                   -> ConduitM ByteString (Either ByteString ByteString) IO r
                   -> (Handles -> IO r)
fuseProcessConduit left right (Handles in1 out2 err) =
  do (in2,out1) <- createHandles
     runConcurrently
       (Concurrently
          (left (Handles in1 out1 err) `finally`
           hClose out1) *>
        Concurrently
          ((sourceHandle in2 $$ right `fuseUpstream`
            sinkHandles out2 err) `finally`
           hClose in2))

-- | Fuse one segment with another.
fuseSegment :: Segment () -> Segment r -> Segment r
SegmentConduit x `fuseSegment` SegmentConduit y =
  SegmentConduit (fuseConduit x y)
SegmentConduit x `fuseSegment` SegmentProcess y =
  SegmentProcess (fuseConduitProcess x y)
SegmentProcess x `fuseSegment` SegmentConduit y =
  SegmentProcess (fuseProcessConduit x y)
SegmentProcess x `fuseSegment` SegmentProcess y =
  SegmentProcess (fuseProcess x y)
