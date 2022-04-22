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

  installPhase = ''
    export HOME=$(mktemp -d)
    elm make src/Main.elm --output $out/elm.js
  '';
}
