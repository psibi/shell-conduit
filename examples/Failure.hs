import Control.Applicative
import Control.Monad.Fix
import Data.Conduit.Binary
import Data.Conduit.Shell

main =
  run (do ls
          echo "Restarting server ... ?"
          killall name "-q" <|> return ()
          fix (\loop ->
                 do echo "Waiting for it to terminate ..."
                    sleep "1"
                    (ps "-C" name >> loop) <|> return ())
          shell "dist/build/ircbrowse/ircbrowse ircbrowse.conf")
  where name = "ircbrowse"
