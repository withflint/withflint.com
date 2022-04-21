{ stdenv
, dotnetCorePackages
, name
}:

stdenv.mkDerivation {
  name = name;

  src = builtins.path {
    path = ./.;
    name = name;
  };

  buildInputs = [
    dotnetCorePackages.sdk_6_0
  ];

  buildPhase = ''
    export HOME=$(mktemp -d)
    dotnet publish --configuration Release -p:ContinuousIntegrationBuild=true -p:Deterministic=true --packages "$HOME/nuget_pkgs"
  '';

  installPhase = ''
    cp -R ./bin/Release/net6.0 $out
  '';
}
