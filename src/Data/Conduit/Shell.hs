{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Shell scripting with Conduit
--
-- This module consists only of re-exports, including a few thousand
-- top-level names based on @PATH@. If you don't want that, you can
-- cherry-pick specific modules to import from the library.
--
-- See "Data.Conduit.Shell.PATH" for all binaries. But you should be
-- able to use whatever executables are in your @PATH@ when the library
-- is compiled.
--
-- == Examples
--
-- The monad instance of Conduit will simply pass along all stdout
-- results:
--
-- Piping with Conduit's normal pipe will predictably pipe things
-- together, as in Bash:
--
-- >>> run (do shell "echo Hello" $| sed "s/l/a/"; echo "OK!")
-- Healo
-- OK!
--
-- Streaming pipes (aka lazy pipes) is also possible:
--
-- >>> run (tail' "/tmp/foo.txt" "-f" $| grep "--line-buffered" "Hello")
-- Hello, world!
-- Oh, hello!
--
-- (Remember that @grep@ needs @--line-buffered@ if it is to output
-- things line-by-line).
--
-- Run custom processes via the @proc@ function:
--
-- >>> run (proc "ls" [])
-- dist  LICENSE  README.md  Setup.hs  shell-conduit.cabal  src  TAGS  TODO.org
--
-- Run shell commands via the @shell@ function:
--
-- >>> run (shell "ls")
-- dist  LICENSE  README.md  Setup.hs  shell-conduit.cabal  src  TAGS  TODO.org
--
-- Run conduits via the @conduit@ function:
--
-- >>> run (cat "/tmp/foo.txt" $| conduit (do Just x <- await; yield x))
-- Hello!
--
-- == How it works
--
-- All executable names in the @PATH@ at compile-time are brought into
-- scope as runnable process conduits e.g. @ls@ or @grep@.
--
--
-- All processes are bound as variadic process calling functions, like this:
--
-- @
-- rmdir :: ProcessType r => r
-- ls :: ProcessType r => r
-- @
--
-- But ultimately the types end up being:
--
-- @
-- rmdir "foo" :: Segment ()
-- ls :: Segment ()
-- ls "." :: Segment ()
-- @
--
-- Etc.
--
-- Run all shell scripts with 'run':
--
-- @
-- run :: Segment r -> IO r
-- @
--
-- == String types
--
-- If using @OverloadedStrings@ so that you can use 'Text' for arguments,
-- then also enable @ExtendedDefaultRules@, otherwise you'll get
-- ambiguous type errors.
--
-- @
-- {-# LANGUAGE ExtendedDefaultRules #-}
-- @
--
-- But this isn't necessary if you don't need to use 'Text' yet. Strings
-- literals will be interpreted as 'String'. Though you can pass a value
-- of type 'Text' or any instance of 'CmdArg' without needing conversions.
--

module Data.Conduit.Shell
  (-- * Running scripts
   run
   -- * Making segments
  ,shell
  ,proc
  ,conduit
  ,text
  ,bytes
  -- * Composition of segments
  ,($|)
  ,Segment
  ,ProcessException(..)
  -- * Re-exports
  -- $exports
  ,module Data.Conduit.Shell.PATH
  ,module Data.Conduit.Shell.Types
  ,module Data.Conduit.Shell.Variadic
  ,module Data.Conduit)
  where

import Data.Conduit
import Data.Conduit.Shell.PATH hiding (strings)
import Data.Conduit.Shell.Process
import Data.Conduit.Shell.Types
import Data.Conduit.Shell.Variadic

-- $exports
--
-- The following modules are exported for scripting
-- convenience. "Data.Conduit" and "Data.Conduit.Filesystem" are
-- re-exported from other libraries because they are typical uses. If
-- you want a stream of the contents of a directory, recursively,
-- 'sourceDirectoryDeep' is handy. A program like @find@ is strict,
-- whereas a Conduit can stop processing whenever you wish.
--
-- You might want to import the regular Conduit modules qualified, too:
--
-- @
-- import qualified Data.Conduit.List as CL
-- @
--
-- Which contains handy functions for working on streams in a
-- list-like way. See the rest of the handy modules for Conduit in
-- conduit-extra: <http://hackage.haskell.org/package/conduit-extra>
--
-- Also of interest is csv-conduit: <http://hackage.haskell.org/package/csv-conduit>
-- And html-conduit: <http://hackage.haskell.org/package/html-conduit>
-- And http-conduit: <http://hackage.haskell.org/package/http-conduit>
--
-- Finally, see the Conduit category on Hackage for other useful libraries: <http://hackage.haskell.org/packages/#cat:Conduit>
--
-- All of these general purpose Conduits can be used in shell
-- scripting.
