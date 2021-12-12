{
  stdenv,
  writeText,
  writeShellScriptBin,

  pkg-config,
  openssl,
  libpng,

  bashInteractive,
  coreutils,
  pstree,

  restless-git,   # the `git save' command

  nodfur-emacs,   # our own Emacs configuration
  nodfur-emacs-packages,

  sbcl,           # Steel Bank Common Lisp
  texlive,        # TeX, LaTeX, XeTeX, etc
  freetype,       # for loading and rendering fonts
  harfbuzz        # for shaping text words
}:

let
  epap-emacs = writeShellScriptBin "epap-emacs" ''
    exec ${nodfur-emacs}/bin/nodfur-emacs boot.lisp --execute "(slime)" "$@"
  '';

in stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    bashInteractive
    coreutils
    pstree

    sbcl
    texlive.combined.scheme-full

    restless-git
    nodfur-emacs
    epap-emacs

    pkg-config
    openssl
    libpng

    # freetype
    # harfbuzz

  ];

  EMACS_SITE_LISP = "${nodfur-emacs-packages.slime}/share/emacs/site-lisp";

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
