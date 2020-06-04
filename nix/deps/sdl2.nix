{ mkDerivation, base, bytestring, deepseq, exceptions, linear, SDL2
, StateVar, stdenv, text, transformers, vector, weigh
}:
mkDerivation {
  pname = "sdl2";
  version = "2.5.2.0";
  sha256 = "9c4110b1e772739bf62c641d375665f9d0af3ad64ed73b24ef407a82e7648fa1";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring exceptions linear StateVar text transformers vector
  ];
  librarySystemDepends = [ SDL2 ];
  libraryPkgconfigDepends = [ SDL2 ];
  testHaskellDepends = [ base deepseq linear vector weigh ];
  description = "Both high- and low-level bindings to the SDL library (version 2.0.6+).";
  license = stdenv.lib.licenses.bsd3;
}
