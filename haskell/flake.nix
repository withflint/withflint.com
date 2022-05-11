{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages = {
          withflint = pkgs.haskell.packages.ghc921.callCabal2nix "withflint" ./. {};
        };

        defaultPackage = self.packages.${system}.withflint;
        
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.ghc921
            haskell.packages.ghc921.fourmolu
            haskell.packages.ghc921.haskell-language-server
            hpack
            cabal-install
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
