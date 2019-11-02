{ pkgs ? import <nixpkgs> {}, fficxxSrc }:

let

  hgdal-src = pkgs.callPackage ./gen.nix { inherit fficxxSrc; };

in

self: super:

{
  "hgdal" = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "hgdal" hgdal-src {}) {
       librarySystemDepends = [pkgs.gdal];
  };
}
