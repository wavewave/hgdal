{ pkgs ? import <nixpkgs> {}, fficxxSrc }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
    };
  };

  stdcxxSrc = import (fficxxSrc + "/stdcxx-gen/gen.nix") {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (fficxxSrc + "/fficxx-runtime") {};
      "fficxx"         = self.callCabal2nix "fficxx"         (fficxxSrc + "/fficxx")         {};
      "stdcxx"         = self.callCabal2nix stdcxxSrc        stdcxxSrc                       {};
    };
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
