# AGENT Instructions for HaskellSMV

This project uses `stack` with resolver `lts-8.0`. To run the test suite you must
install `stack` and then execute `stack test`.

Steps:
1. Ensure the `haskell-stack` package is installed. On Debian/Ubuntu you can run:
   ```bash
   apt-get update && apt-get install -y haskell-stack
   ```
2. Run the tests:
   ```bash
   stack test
   ```
   The first run downloads GHC 8.0.2 and many dependencies which can take a while.

After running `stack test` the working directory may contain generated files
such as `stack.yaml.lock` and a modified `HaskellSMV.cabal`. Use `git checkout -- HaskellSMV.cabal` and remove `stack.yaml.lock` before committing.

