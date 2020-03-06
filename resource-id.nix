{ mkDerivation, base, base64-bytestring, bytestring, hpack
, pretty-terminal, stdenv, text
}:
mkDerivation {
  pname = "resource-id";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring pretty-terminal text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base base64-bytestring bytestring pretty-terminal text
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring pretty-terminal text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/yuanwang-wf/resource-id#readme";
  license = stdenv.lib.licenses.bsd3;
}
