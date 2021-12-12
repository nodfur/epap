{
  stdenv,
  pkg-config,
  openssl,

  sbcl,       # Steel Bank Common Lisp
  texlive,    # TeX, LaTeX, XeTeX, etc
  freetype,   # for loading and rendering fonts
  harfbuzz    # for shaping text words
}:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    pkg-config
    sbcl
    openssl
    texlive.combined.scheme-full
    freetype
    harfbuzz
  ];
  preBuild = ''
    export HOME=$TMPDIR
  '';
  installPhase = ''
    # zig build --prefix $out install
  '';
}
