{ stdenv, zig }:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [zig];
  preBuild = ''
    export HOME=$TMPDIR
  '';
  installPhase = ''
    zig build --prefix $out install
  '';
}
