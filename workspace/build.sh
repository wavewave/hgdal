rm -rf dist-newstyle
cabal build fficxx
cabal exec runhaskell ../../fficxx/stdcxx-gen/Gen.hs
cabal build stdcxx
cabal exec -- ghc ../Gen.hs -package optparse-applicative
../Gen gen
cabal build hgdal
cabal exec -- ghc example.hs  -package monad-loops  -lstdc++
