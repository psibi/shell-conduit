{-# LANGUAGE ExtendedDefaultRules #-}

import Test.Hspec
import Data.Conduit.Shell

-- Ensures that true and false functions are generated from TH
import Data.Conduit.Shell.PATH (true, false)
import Data.Conduit.Shell.Segments (strings)
import Test.Hspec.Expectations
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
