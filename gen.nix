{ pkgs }:

with pkgs;

let

  hsenv =
    haskellPackages.ghcWithPackages (p: with p; [ fficxx fficxx-runtime ]);

  gen = stdenv.mkDerivation {
    name = "hgdal-src";
    buildInputs = [ hsenv ];
    src = ./.;
    buildPhase = ''
      ghc Gen.hs
      ./Gen ./template
    '';
    installPhase = ''
      mkdir -p $out
      cp -a hgdal/* $out
    '';
  };

in gen
