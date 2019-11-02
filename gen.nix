{ pkgs ? import <nixpkgs> {}, fficxxSrc }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = callPackage ./nix/config.nix { inherit fficxxSrc; };
  };

  stdcxxNix = import ./nix/config-stdcxx.nix {
    inherit fficxxSrc stdenv fetchgit; packages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      callPackage ./nix/config.nix { inherit fficxxSrc; } self super //
      { "stdcxx" = self.callPackage stdcxxNix {}; };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    fficxx
    fficxx-runtime
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
