cabal clean
cabal exec -- ghc ../Gen.hs -package optparse-applicative
../Gen gen
cabal build hgdal
cabal exec -- ghc example.hs  -package monad-loops  -lstdc++
