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
