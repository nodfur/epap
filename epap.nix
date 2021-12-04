{ stdenv, pkg-config, zig, libcap, sbcl, lispPackages }:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    pkg-config
    zig
    libcap
    sbcl
    lispPackages.cffi
    lispPackages.alexandria
    lispPackages.trivial-features
    lispPackages.babel
  ];
  preBuild = ''
    export HOME=$TMPDIR
  '';
  installPhase = ''
    zig build --prefix $out install
  '';
}
