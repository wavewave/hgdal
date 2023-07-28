rm -rf hgdal
rm -rf tmp
cabal clean
ghc ../Gen.hs
../Gen gen
cabal build hgdal
cabal exec -- ghc example.hs  -package monad-loops -package vector -lstdc++
