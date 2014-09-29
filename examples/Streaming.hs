import Data.Conduit.Shell

main = run (do tail' "/tmp/example.txt" "-f" $| grep "--line-buffered" "Hello")
