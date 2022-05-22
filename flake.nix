{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          ghcVersion = "ghc922";
          haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              retry = pkgs.haskell.lib.dontCheck super.retry;
            };
          };
          gitVersion = if (self ? shortRev) then self.shortRev else "dirty";
      in {
        devShell = haskellPackages.shellFor {
          packages = pkgs: [
            self.packages.${system}.backend
          ];
          
          buildInputs = with pkgs; [
            elmPackages.elm-review
            elmPackages.elm-format
            elmPackages.elm-live
            elmPackages.elm
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.fourmolu_0_6_0_0
            cabal2nix
          ];
        };

        apps = rec {
          default = withflint;
          
          withflint = {
            type = "app";
            program = "${self.packages.${system}.withflint}/bin/withflint";
          };
        };

        packages = rec {
          withflint-image = pkgs.dockerTools.buildImage {
            name = "withflint";
            contents = [ withflint ];
            created = "now";
            tag = "latest";
            config = {
              EntryPoint = [ "/bin/withflint" ];
            };
          };
          
          default = withflint;
          
          backend = haskellPackages.callPackage ./haskell/default.nix {
            name = "withflint-backend";
          };

          frontend = pkgs.callPackage ./elm/default.nix {
            name = "withflint-frontend";
          };

          withflint = pkgs.callPackage ./default.nix {
            inherit frontend gitVersion;
            backend = pkgs.haskell.lib.justStaticExecutables backend;
            name = "withflint";
          };
        };
      }
    );
}
