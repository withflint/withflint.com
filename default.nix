{ stdenv
, makeWrapper
, gitVersion
, backend
, frontend
, name
}:

stdenv.mkDerivation {
  name = name;

  src = builtins.path {
    path = ./.;
    name = name;
  };

  buildInputs = [
    backend
    frontend
  ];

  nativeBuildInputs = [
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out $out/bin
    cp -R ./static ./blog ./jobs index.html $out/
    mkdir $out/static/${gitVersion}

    sed -i "s/GIT_VERSION/${gitVersion}/g" $out/index.html

    makeWrapper ${backend}/backend $out/bin/backend \
      --set WITHFLINT_ROOT $out \
      --set GIT_VERSION ${gitVersion}

    cp ${frontend}/elm.js $out/static/${gitVersion}/elm.js
  '';
}
