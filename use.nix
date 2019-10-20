{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      callPackage ./nix/config.nix { } self super //
      callPackage ./default.nix { } self super;
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    hgdal
    monad-loops
  ]);

in

stdenv.mkDerivation {
  name = "hgdal-env";

  buildInputs = [
    hsenv
  ];

}
