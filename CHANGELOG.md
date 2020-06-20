# 5.0.0

* Generalize from IO to MonadUnliftIO m => m. See [PR 17](https://github.com/psibi/shell-conduit/pull/17)
* More detter exclusion of disallowed names.

# 4.7.0

* Port it for newer conduit and resourcet

# 4.6.2

* Add test for piping feature

# 4.6.1

* Fix import error in Stackage: https://github.com/fpco/stackage/issues/2355

# 4.6.0

* Add basic tests code
* Accept list as variadic command line arguments.
  `mkdir "-p" ["folder1", "folder2"]` works now.
* TRAVIS CI added
