{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generate top-level names for binaries.

module Data.Conduit.Shell.TH
  (generateBinaries)
  where

import Data.Conduit.Shell.Variadic

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Language.Haskell.TH
import System.Directory
import System.Environment
import System.FilePath

-- | Generate top-level names for all binaries in PATH.
generateBinaries :: Q [Dec]
generateBinaries =
  do bins <- runIO getAllBinaries
     mapM (\(name,bin) ->
             do uniqueName <- getUniqueName name
                return (FunD uniqueName
                             [Clause []
                                     (NormalB (AppE (VarE 'variadicProcess)
                                                    (LitE (StringL bin))))
                                     []]))
          (nubBy (on (==) fst)
                 (filter (not . null . fst)
                         (map (normalize &&& id) bins)))
  where normalize = uncapitalize . go
          where go (c:cs)
                  | c == '-' || c == '_' =
                    case go cs of
                      (z:zs) -> toUpper z : zs
                      [] -> []
                  | not (elem (toLower c) allowed) = go cs
                  | otherwise = c : go cs
                go [] = []
        uncapitalize (c:cs)
          | isDigit c = '_' : c : cs
          | otherwise = toLower c : cs
        uncapitalize [] = []
        allowed =
          ['a' .. 'z'] ++
          ['0' .. '9']

-- | Get a version of the given name available to be bound.
getUniqueName :: String -> Q Name
getUniqueName candidate =
  do inScope <- recover (return False)
                        (do void (reify (mkName candidate))
                            return True)
     if inScope || candidate == "import" || candidate == "type"
        then getUniqueName (candidate ++ "'")
        else return (mkName candidate)

-- | Get a list of all binaries in PATH.
getAllBinaries :: IO [FilePath]
getAllBinaries =
  do path <- getEnv "PATH"
     fmap concat
          (forM (splitOn ":" path)
                (\dir ->
                   do exists <- doesDirectoryExist dir
                      if exists
                         then do contents <- getDirectoryContents dir
                                 filterM (\file ->
                                            do exists' <- doesFileExist (dir </> file)
                                               if exists'
                                                  then do perms <- getPermissions (dir </> file)
                                                          return (executable perms)
                                                  else return False)
                                         contents
                         else return []))
