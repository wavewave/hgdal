{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages = haskellPackages.override {
    overrides = pkgs.callPackage ./nix/config.nix {};
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    fficxx-runtime fficxx
  ]);

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

in

gen
