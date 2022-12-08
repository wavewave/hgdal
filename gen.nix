{ stdenv }:
hself:

let

  hsenv = hself.ghcWithPackages
    (p: with p; [ fficxx fficxx-runtime optparse-applicative ]);

in stdenv.mkDerivation {
  name = "hgdal-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    ghc Gen.hs
    ./Gen gen -t ./template
  '';
  installPhase = ''
    mkdir -p $out
    cp -a hgdal/* $out
  '';
}

