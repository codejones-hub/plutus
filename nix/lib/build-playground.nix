{ lib, buildPursProject, runCommand, makeWrapper, nodejs-headers, easyPS, webCommon }:

{ srcDir
, name
, packageJSON ? srcDir + "/package.json"
, yarnLock ? srcDir + "/yarn.lock"
, yarnNix ? srcDir + "/yarn.nix"
, additionalPurescriptSources ? [ "../web-common/**/*.purs" ]
, pscPackages
, spagoPackages
, runtimeGhc
, playground-exe
, playground-exe-name
}:
let
  targetExeName = lib.removeSuffix "-server" playground-exe-name;
  server-invoker = runCommand "playground-invoker" { buildInputs = [ makeWrapper ]; } ''
    mkdir -p $out/bin
    ln -s ${playground-exe}/bin/${playground-exe-name} $out/bin/${targetExeName}
    wrapProgram $out/bin/${targetExeName} \
      --set GHC_LIB_DIR "${runtimeGhc}/lib/ghc-${runtimeGhc.version}" \
      --set GHC_BIN_DIR "${runtimeGhc}/bin" \
      --set GHC_PACKAGE_PATH "${runtimeGhc}/lib/ghc-${runtimeGhc.version}/package.conf.d" \
      --set GHC_RTS "-M2G"
  '';
  generated = runCommand "${name}-purescript" { } ''
    mkdir $out
    ${playground-exe}/bin/${playground-exe-name} psgenerator $out
  '';
in
buildPursProject rec {
  inherit name packageJSON yarnLock yarnNix;
  inherit additionalPurescriptSources;
  inherit spagoPackages;

  src = srcDir;
  psSrc = generated;
  packages = pscPackages;
}
