rm -rf dist-newstyle
runhaskell ../Gen.hs
cabal new-build hgdal
cabal new-exec -- ghc example.hs  -package monad-loops  -lstdc++
