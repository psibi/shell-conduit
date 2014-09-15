shell-conduit
=====

Write shell scripts with Conduit. Still in the experimental phase.

### What is this?

All executable names in the `PATH` at compile-time are brought into
scope as runnable process conduits e.g. `ls` or `grep`.

Stdin/out and stderr are handled as an Either type.

``` haskell
type Chunk = Either ByteString ByteString
```

`Left` is stderr, `Right` is stdin/stdout.

### File example

``` haskell
-- | Download and initialize repo.

import Control.Monad.IO.Class
import Data.Conduit.Shell
import System.Directory

main =
  run (do exists <- liftIO (doesDirectoryExist "repo")
          if exists
             then rm ["repo/.hsenv","-rf"]
             else git ["clone","git@github.com:chrisdone/repo.git"]
          liftIO (setCurrentDirectory "repo")
          shell "./scripts/update-repo"
          shell "./scripts/build-all"
          alertDone [])
```

### REPL example

Some imports:

``` haskell
λ> import Data.Char
λ> import Data.Conduit
λ> import Data.Conduit.Shell
λ> import qualified Data.Conduit.Shell.Combinators as SH
λ> import qualified Data.ByteString.Char8 as S8
```

Piping:

``` haskell
λ> run (ls [] $= grep ["Key"] $= shell "cat" $= SH.map (S8.map toUpper))
KEYBOARD.HI
KEYBOARD.HS
KEYBOARD.O
```

Running actions in sequence and piping:

``` haskell
λ> run (do shell "echo sup"; shell "echo hi")
sup
hi
λ> run (do shell "echo sup"; sed ["s/u/a/"]; shell "echo hi")
sup
hi
λ> run (do shell "echo sup" $= sed ["s/u/a/"]; shell "echo hi")
sap
hi
```

Streaming:

``` haskell
λ> run (do tail' ["/tmp/example.txt","-f"] $= grep ["--line-buffered","Hello"])
Hello, world!
Oh, hello!
```

(Remember that `grep` needs `--line-buffered` if it is to output things
line-by-line).
