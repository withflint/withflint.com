{ mkDerivation
, name
, aeson
, base
, blaze-builder
, bytestring
, containers
, directory
, filepath
, lens
, lib
, lucid
, mime-mail
, mtl
, network
, parsec
, scotty
, shakespeare
, smtp-mail
, text
, time
, wai
, wai-extra
, wai-middleware-static
, warp
}:

mkDerivation {
  pname = name;
  version = "0.1.0.0";
  
  src = ./.;
  
  isLibrary = true;
  isExecutable = true;
  
  libraryHaskellDepends = [
    aeson
    base
    blaze-builder
    bytestring
    containers
    directory
    filepath
    lens
    lucid
    mime-mail
    mtl
    network
    parsec
    scotty
    shakespeare
    smtp-mail
    text
    time
    wai
    wai-extra
    wai-middleware-static
    warp
  ];
  
  executableHaskellDepends = [
    base
  ];
  
  doHaddock = false;
  
  license = "unknown";
  
  hydraPlatforms = lib.platforms.none;
}
