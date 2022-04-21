{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          gitVersion = if (self ? rev) then self.rev else "dirty";
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            elmPackages.elm-review
            elmPackages.elm-format
            elmPackages.elm-live
            elmPackages.elm
            dotnetCorePackages.sdk_6_0
          ];
        };

        defaultPackage = self.packages.${system}.withflint;
        defaultApp = self.apps.${system}.withflint;
        
        apps = {
          withflint = {
            type = "app";
            program = "${self.packages.${system}.withflint}/bin/backend";
          };
        };

        packages = rec {
          backend = pkgs.callPackage ./backend/default.nix {
            name = "withflint-backend";
          };

          frontend = pkgs.callPackage ./elm/default.nix {
            name = "withflint-frontend";
          };

          withflint = pkgs.callPackage ./default.nix {
            inherit backend frontend gitVersion;
            name = "withflint";
          };
        };
      }
    );
}