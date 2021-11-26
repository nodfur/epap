{ stdenv, zig }:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [zig];
  installPhase = ''
    mkdir $out/bin
    cp epap $out/bin
  '';
}
