rm -rf dist-newstyle
cd ..; ghc Gen.hs ; cd workspace
../Gen
cabal new-build hgdal
cabal new-exec -- ghc example.hs  -package monad-loops  -lstdc++
