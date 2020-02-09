{ mkDerivation, base, dhall, hpack, stdenv, text }:
mkDerivation {
  pname = "plananalysis";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base dhall text ];
  testHaskellDepends = [ base dhall text ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/plananalysis#readme";
  license = stdenv.lib.licenses.bsd3;
}
