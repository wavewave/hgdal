{
  description = "hgdal";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, fficxx }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellOverlay = final: hself: hsuper:
          (import ./default.nix { pkgs = final; } hself hsuper);

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper
              // haskellOverlay pkgs hself hsuper));

        # TODO: use haskell.packages.(ghc).shellFor
        mkShellFor = compiler:
          let
            hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
              p.fficxx
              p.fficxx-runtime
              p.stdcxx
              p.monad-loops
              p.optparse-applicative
              p.dotgen
            ]);
          in pkgs.mkShell {
            buildInputs = [
              hsenv
              pkgs.cabal-install
              pkgs.gdal
              pkgs.pkgconfig
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.graphviz
            ];
            shellHook = "";
          };

        supportedCompilers = [ "ghc902" "ghc924" "ghc942" ];

      in {
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

        inherit haskellOverlay;

        devShells =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);

      });
}
