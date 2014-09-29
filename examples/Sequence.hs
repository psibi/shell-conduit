import Data.Conduit.Shell

main = do run (do shell "echo sup"; shell "echo hi")
          run (do shell "echo sup" $| sed "s/u/a/"; shell "echo hi")
