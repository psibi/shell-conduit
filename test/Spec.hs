{-# LANGUAGE ExtendedDefaultRules #-}

import Test.Hspec
import Data.Conduit.Shell
import Data.Conduit.Shell.PATH (true, false)
import Data.Conduit.Shell.Segments (strings, ignore)
import Control.Applicative

main :: IO ()
main =
  hspec $
  do describe "SHELL path functions" $
       do it "false" $
            do val <- run $ strings (false <|> echo "failed")
               val `shouldBe` ["failed"]
          it "true" $
            do val <- run $ strings (true <|> echo "passed")
               val `shouldBe` []
     describe "ls" $
       do it "home directory check" $
            do val <- run $ strings (ls "/")
               val `shouldContain` ["home"]
          it "long option" $
            do val <- run $ strings (ls "-a" ["/"])
               val `shouldContain` ["home"]
     describe "multiple string usage" $
       do it "make two directory" $
            do val <-
                 run $
                 do ignore $ mkdir "-p" "mtest1" "mtest2" "mtest3"
                    strings $ ls "."
               run $ rmdir ["mtest1", "mtest2", "mtest3"]
               val `shouldContain` ["mtest1", "mtest2", "mtest3"]
     describe "list usage in variadic" $
       do it "two directory" $
            do val <-
                 run $
                 do ignore $ mkdir "-p" ["test1", "test2"]
                    strings $ ls "."
               run $ rmdir ["test1", "test2"]
               val `shouldContain` ["test1", "test2"]
     describe "shell calls" $
       do it "shell ls" $
            do val <- run $ do strings $ shell "ls /"
               val `shouldContain` ["home"]
     describe "cd" $
       do it "cd /" $
            do val <-
                 run $
                 do ignore $ cd "/"
                    strings pwd
               val `shouldBe` ["/"]
          it "cd /home" $
            do val <-
                 run $
                 do ignore $ cd ["/home", undefined]
                    strings pwd
               val `shouldBe` ["/home"]
