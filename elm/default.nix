{ stdenv
, elmPackages
, name
}:

stdenv.mkDerivation {
  name = name;

  src = builtins.path {
    path = ./.;
    name = name;
  };

  buildInputs = with elmPackages; [
    elm
  ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import ./packages.nix;

    elmVersion = "0.19.1";
    
    registryDat = ./registry.dat;
  };

  installPhase = ''
    elm make src/Main.elm --optimize --output $out/elm.js
  '';
}
