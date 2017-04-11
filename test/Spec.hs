{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

import Test.Hspec
import Data.Conduit.Shell hiding (ignore) -- https://github.com/fpco/stackage/issues/2355#issue-212177275
import Data.Conduit.Shell.PATH (true, false)
import Data.Conduit.Shell.Segments (strings, ignore)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as S8
import Control.Applicative
import Data.ByteString
import Data.Char (toUpper)

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
     describe "ordering of arguments" $
       do it "echo -e" $
            do val <- run $ do strings $ echo "-e" "hello\n" "haskell"
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell"]
#else
               val `shouldBe` ["hello", " haskell"]
#endif
          it "mixed variant" $
            do val <- run $ strings $ echo "-e" ["hello\n", "haskell"]
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell"]
#else
               val `shouldBe` ["hello", " haskell"]
#endif
          it "list variant" $
            do val <- run $ strings $ echo ["-e", "hello\n", "haskell"]
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell"]
#else
               val `shouldBe` ["hello", " haskell"]
#endif
          it "list mixed variant - 1" $
            do val <- run $ strings $ echo "-e" ["hello\n", "haskell"]
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell"]
#else
               val `shouldBe` ["hello", " haskell"]
#endif
          it "list mixed variant - 2" $
            do val <- run $ strings $ echo "-e" ["hello\n", "haskell\n"] "world"
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell", " world"]
#else
               val `shouldBe` ["hello", " haskell", " world"]
#endif
          it "list mixed variant - 3" $
            do val <- run $ strings $ echo "-e" ["hello\n", "haskell\n"] "world\n" ["planet"]
#ifdef darwin_HOST_OS
               val `shouldBe` ["-e hello", " haskell", " world", " planet"]
#else
               val `shouldBe` ["hello", " haskell", " world", " planet"]
#endif
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
     describe "Piping" $
       do it "basic piping" $
            do (val :: [String]) <-
                 run $ strings (echo "hello" $| conduit (CL.map (S8.map toUpper)))
               val `shouldBe` ["HELLO"]
          it "piping of commands - example 1" $
             do val <- run $ strings (ls "/" $| grep "etc")
                val `shouldBe` ["etc"]
          it "piping of commands - example 2" $
             do val <- run $ strings (echo "hello" $| tr "[a-z]" "[A-Z]")
                val `shouldBe` ["HELLO"]
