{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };

          ghcVersion = "ghc922";

          haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
            overrides = self: super: {
              retry = pkgs.haskell.lib.dontCheck super.retry;

              fourmolu = super.fourmolu_0_6_0_0;

              ghcid = pkgs.haskell.lib.overrideCabal super.ghcid (drv: {
                enableSeparateBinOutput = false;
              });
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
            elm2nix
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.cabal2nix
            haskellPackages.ghcid
            self.packages.${system}.fix-script
            self.packages.${system}.update-elm-script
            self.packages.${system}.dev-script
            self.packages.${system}.format-script
          ];
        };

        apps = rec {
          default = withflint;

          withflint = {
            type = "app";
            program = "${self.packages.${system}.withflint-script}";
          };
        };

        packages = rec {
          withflint-image = pkgs.dockerTools.buildImage {
            name = "withflint";

            contents = [
              withflint
              pkgs.glibcLocales
              pkgs.cacert
              pkgs.busybox
              pkgs.bash
            ];

            created = "now";

            tag = "latest";

            config = {
              EntryPoint = [ "/bin/withflint" ];
              Env = [
                "LANG=en_US.UTF-8"
                "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
              ];
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

          withflint-script = pkgs.writeShellScript "withflint.sh" ''
            if test -f ".env"; then
              source .env
            fi

            ${withflint}/bin/withflint
          '';

          dev-script = pkgs.writeShellScriptBin "dev" ''
            if [ -f "flake.nix" ]; then
              ${pkgs.elmPackages.elm-format}/bin/elm-format elm/src --yes
              ${haskellPackages.fourmolu}/bin/fourmolu -e --mode inplace $(git ls-files '*.hs')

              mkdir static/dirty
              cp elm/src/app.js static/dirty/app.js
              ${pkgs.concurrently}/bin/concurrently \
                -n elm,haskell \
                -c cyan,magenta \
                "cd elm && ${pkgs.elmPackages.elm-live}/bin/elm-live --no-server --path-to-elm ${pkgs.elmPackages.elm}/bin/elm src/Main.elm -- --output=../static/dirty/elm.js" \
                "cd haskell && ${haskellPackages.ghcid}/bin/ghcid --run=dev"

              rm static/dirty/elm.js
              rm static/dirty/app.js
              rmdir static/dirty
            else
              echo "Please run this script from the root directory! (the one with flake.nix)"
            fi
          '';

          update-elm-script = pkgs.writeShellScriptBin "update-elm" ''
            if [ -f "elm.json" ]; then
              ${pkgs.elm2nix}/bin/elm2nix convert > packages.nix
              ${pkgs.elm2nix}/bin/elm2nix snapshot > registry.dat
            else
              echo "Please run this script from the Elm project directory! (the one with elm.json)"
            fi
          '';

          format-script = pkgs.writeShellScriptBin "format" ''
            if [ -f "flake.nix" ]; then
              ${pkgs.elmPackages.elm-format}/bin/elm-format elm/src --yes
              ${haskellPackages.fourmolu}/bin/fourmolu -e --mode inplace $(git ls-files '*.hs')
            else
              echo "Please run this script from the root directory! (the one with flake.nix)"
            fi
          '';

          fix-script = pkgs.writeShellScriptBin "fix" ''
            if [ -f "flake.nix" ]; then
              ${pkgs.elmPackages.elm-review}/bin/elm-review --compiler ${pkgs.elmPackages.elm}/bin/elm --fix-all --elmjson elm/elm.json
            else
              echo "Please run this script from the root directory! (the one with flake.nix)"
            fi
          '';
        };
      }
    );
}
