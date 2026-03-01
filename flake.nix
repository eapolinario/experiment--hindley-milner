{
  description = "Hindley-Milner type inference engine in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Pick a concrete GHC version for reproducibility
        hpkgs = pkgs.haskell.packages.ghc967;

        # GHC bundled with the library dependencies the project needs
        ghcWithDeps = hpkgs.ghcWithPackages (ps: with ps; [
          containers  # Data.Map, Data.Set
          mtl         # Control.Monad.State, Control.Monad.Except
        ]);
      in {
        # --- Development shell ---
        # Enter with:  nix develop
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghcWithDeps
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.hlint
            hpkgs.ormolu          # code formatter
            pkgs.ghcid            # fast recompile loop: ghcid --command "cabal repl"
            pkgs.just             # task runner (see justfile)
          ];

          shellHook = ''
            echo "Hindley-Milner dev environment"
            echo "  ghc:   $(ghc --version)"
            echo "  cabal: $(cabal --version)"
            echo ""
            echo "Useful commands:"
            echo "  just                 -- list all dev tasks"
            echo "  just build           -- compile"
            echo "  just test            -- run tests"
            echo "  just repl            -- interactive REPL"
            echo "  just watch           -- live-reloading watcher"
          '';
        };
      }
    );
}
