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
      in {
        packages = {
          withflint = haskellPackages.callCabal2nix "withflint" ./. {};
          default = self.packages.${system}.withflint;
        };
        
        devShell = haskellPackages.shellFor {
          packages = pkgs: [
            self.packages.${system}.withflint
          ];

          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.fourmolu_0_6_0_0
            pkgs.cabal2nix
          ];
        };
      }
    );
}
