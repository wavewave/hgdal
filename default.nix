{ pkgs ? import <nixpkgs> {} }:

let

  hgdal-src = pkgs.callPackage ./gen.nix {};

in

self: super:

{
  "hgdal" = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "hgdal" hgdal-src {}) {
       librarySystemDepends = [pkgs.gdal];
  };
}
