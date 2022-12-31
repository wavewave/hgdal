{
  description = "hgdal";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellOverlay = final: hself: hsuper:
          (import ./default.nix { pkgs = final; } hself hsuper);

        fficxx-version = "0.7.0.0";

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            {
              "fficxx" = hself.callHackage "fficxx" fficxx-version { };
              "fficxx-runtime" =
                hself.callHackage "fficxx-runtime" fficxx-version { };
              "stdcxx" = hself.callHackage "stdcxx" fficxx-version { };
              "template" = pkgs.haskell.lib.doJailbreak hsuper.template;
              "ormolu" = pkgs.haskell.lib.overrideCabal hsuper.ormolu
                (drv: { enableSeparateBinOutput = false; });
            } // haskellOverlay pkgs hself hsuper);

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
              pkgs.graphviz
              # this is due to https://github.com/NixOS/nixpkgs/issues/140774
              (hpkgsFor "ghc924").ormolu
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
