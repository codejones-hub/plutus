{ stdenv, fetchurl, lib }:

stdenv.mkDerivation rec {
  pname = "purty";
  version = "6.2.1";

  src = fetchurl {
    url = "https://registry.npmjs.org/purty/-/purty-${version}.tgz";
    sha256 = "17i3sdafcy87p6giivnsj29hzmsgaqbmgs81im8c3nc96ggqjfdr";
  };

  buildPhase = ":";

  installPhase = ''
    find .
    mkdir -p $out/bin
  '' + lib.optionalString (stdenv.isLinux) "cp ./bin/linux/purty $out/bin"
  + lib.optionalString (stdenv.isDarwin) "cp ./bin/osx/purty $out/bin";
}
