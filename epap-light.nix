{
  stdenv,

  pkg-config,
  openssl,
  libpng,

  sbcl,
  zig
}:

stdenv.mkDerivation {
  name = "epap-light";
  version = "0.5";
  src = ./.;
  buildInputs = [
    pkg-config
    openssl
    libpng
    sbcl
    zig
  ];

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
