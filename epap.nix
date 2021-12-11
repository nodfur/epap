{
  stdenv,
  pkg-config,
  openssl,

  zig,        # for building C/C++ and low-level functions
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
    sbcl
    openssl
    texlive.combined.scheme-full
    imagemagick
  ] ++ (if zig == null then [] else [zig]);
  preBuild = ''
    export HOME=$TMPDIR
  '';
  installPhase = ''
    zig build --prefix $out install
  '';
}
