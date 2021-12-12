{
  stdenv,
  pkg-config,
  openssl,

  restless-git,   # the `git save' command

  nodfur-emacs,   # our own Emacs configuration
  nodfur-emacs-packages,

  sbcl,           # Steel Bank Common Lisp
  texlive,        # TeX, LaTeX, XeTeX, etc
  freetype,       # for loading and rendering fonts
  harfbuzz        # for shaping text words
}:

stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    sbcl
    texlive.combined.scheme-full

    restless-git
    nodfur-emacs

    pkg-config
    openssl
    freetype
    harfbuzz
  ];

  EMACS_SITE_LISP = "${nodfur-emacs-packages.slime}/share/emacs/site-lisp";

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
