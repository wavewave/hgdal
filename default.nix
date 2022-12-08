{ pkgs }:

hself: hsuper:

let

  hgdal-src = (pkgs.callPackage ./gen.nix { }) hself;

in {
  "hgdal" =
    pkgs.haskell.lib.overrideCabal (hself.callCabal2nix "hgdal" hgdal-src { }) {
      librarySystemDepends = [ pkgs.gdal ];
    };
}
