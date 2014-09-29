import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Shell

main = run (ls $| grep "Key" $| shell "cat" $| conduit (CL.map (S8.map toUpper)))
