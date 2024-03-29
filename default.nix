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
    cp -R ./static ./content $out/
    mkdir $out/static/${gitVersion}

    makeWrapper ${backend}/bin/withflint $out/bin/withflint \
      --set WITHFLINT_ROOT $out \
      --set GIT_VERSION ${gitVersion}

    ln -s ${frontend}/elm.js $out/static/${gitVersion}/elm.js
    ln -s ${frontend}/app.js $out/static/${gitVersion}/app.js
  '';
}
