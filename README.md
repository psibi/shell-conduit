shell-conduit [![Hackage](https://img.shields.io/hackage/v/shell-conduit.svg?style=flat)](https://hackage.haskell.org/package/shell-conduit)
=====

Write shell scripts with Conduit. Still in the experimental phase.

[Haddock API documentation](http://chrisdone.github.io/shell-conduit/).

### Examples

##### Cloning and initializing a repo

``` haskell
import Control.Monad.IO.Class
import Data.Conduit.Shell
import System.Directory

main =
  run (do exists <- liftIO (doesDirectoryExist "fpco")
          if exists
             then rm "fpco/.hsenvs" "-rf"
             else git "clone" "git@github.com:fpco/fpco.git"
          liftIO (setCurrentDirectory "fpco")
          shell "./dev-scripts/update-repo.sh"
          shell "./dev-scripts/build-all.sh"
          alertDone)
```

##### Piping

Piping of processes and normal conduits is possible:

``` haskell
λ> run (ls $= grep "Key" $= shell "cat" $= CL.map (second (S8.map toUpper)))
KEYBOARD.HI
KEYBOARD.HS
KEYBOARD.O
```

##### Running actions in sequence and piping

Results are outputted to stdout unless piped into other processes:

``` haskell
λ> run (do shell "echo sup"; shell "echo hi")
sup
hi
λ> run (do shell "echo sup"; sed "s/u/a/"; shell "echo hi")
sup
hi
λ> run (do shell "echo sup" $= sed "s/u/a/"; shell "echo hi")
sap
hi
```

##### Streaming

Live streaming between pipes like in normal shell scripting is
possible:

``` haskell
λ> run (do tail' "/tmp/example.txt" "-f" $= grep "--line-buffered" "Hello")
Hello, world!
Oh, hello!
```

(Remember that `grep` needs `--line-buffered` if it is to output things
line-by-line).

##### Handling exit failures

Process errors can be ignored by using the Alternative instance.

``` haskell
import Control.Applicative
import Control.Monad.Fix
import Data.Conduit.Shell

main =
  run (do ls
          echo "Restarting server ... ?"
          killall name "-q" <|> return ()
          fix (\loop ->
                 do echo "Waiting for it to terminate ..."
                    sleep "1"
                    (ps "-C" name $= discardChunks >> loop) <|> return ())
          shell "dist/build/ircbrowse/ircbrowse ircbrowse.conf")
  where name = "ircbrowse"
```

##### Keyboard configuration

``` haskell
import Data.Conduit.Shell
main =
  run (do xmodmap ".xmodmap"
          xset "r" "rate" "150" "50")
```

### How it works

All executable names in the `PATH` at compile-time are brought into
scope as runnable process conduits e.g. `ls` or `grep`.

Stdin/out and stderr are handled as an Either type.

``` haskell
type Chunk = Either ByteString ByteString
```

`Left` is stderr, `Right` is stdin/stdout.

All processes are bound as variadic process calling functions, like this:

``` haskell
rmdir :: ProcessType r => r
ls :: ProcessType r => r
```

But ultimately the types end up being:

``` haskell
rmdir "foo" :: Conduit Chunk m Chunk
ls :: Conduit Chunk m Chunk
ls "." :: Conduit Chunk m Chunk
```

Etc.

Run all shell scripts with

``` haskell
run :: (MonadIO m, MonadBaseControl IO m)
    => Conduit Chunk (ShellT m) Chunk -> m ()
```

The `ShellT` type has a handy `Alternative` instance and can store
info like whether to echo all processes run similar to Bash's `set -x`.

### String types

If using `OverloadedStrings` so that you can use `Text` for arguments,
then also enable `ExtendedDefaultRules`, otherwise you'll get
ambiguous type errors.

``` haskell
{-# LANGUAGE ExtendedDefaultRules #-}
```

But this isn't necessary if you don't need to use `Text` yet. Strings
literals will be interpreted as `String`. Though you can pass a value
of type `Text` or any instance of `CmdArg` without needing conversions.

### Other modules

You might want to import the regular Conduit modules qualified, too:

``` haskell
import qualified Data.Conduit.List as CL
```

Which contains handy functions for working on streams in a
list-like way. See the rest of the handy modules for Conduit in
[conduit-extra](http://hackage.haskell.org/package/conduit-extra).

Also of interest is
[csv-conduit](http://hackage.haskell.org/package/csv-conduit),
[html-conduit](http://hackage.haskell.org/package/html-conduit), and
[http-conduit](http://hackage.haskell.org/package/http-conduit).

Finally, see the Conduit category on Hackage for other useful
libraries: <http://hackage.haskell.org/packages/#cat:Conduit>

All of these general purpose Conduits can be used in shell
scripting.
