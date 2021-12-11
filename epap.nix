{
  stdenv,
  pkg-config,
  openssl,

  zig,        # for building C/C++ and low-level functions
  libcap,     # for manipulating Linux capabilities
  sbcl,       # Steel Bank Common Lisp
  texlive,    # for TeX, LaTeX, XeTeX, etc
  imagemagick # for resizing images, etc
}:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    pkg-config
    zig
    libcap
    sbcl
    openssl
    texlive.combined.scheme-medium
    imagemagick
  ];
  preBuild = ''
    export HOME=$TMPDIR
  '';
  installPhase = ''
    zig build --prefix $out install
  '';
}
