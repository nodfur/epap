{
  stdenv,
  writeText,
  writeShellScriptBin,

  pkg-config,
  openssl,

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
  bashrc = writeText "epap-bashrc" ''
    if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
      PS1=$'\e[93m\]epap\e[0m\] \[\e[1m\]\h\[\e[0m\]:\w\[\e[1m\]`eval "$PS1GIT"`\[\e[0m\]\$ '
      PS1GIT='[[ `git status --short 2>/dev/null` ]] && echo \*'
      [[ $TERM = xterm* ]] && PS1='\[\033]2;\h:\w\007\]'"$PS1"
    fi

    export PATH=$(pwd)/bin:$PATH

    echo
    echo ";;; This is the shell environment for epap."
    echo ";;; Use \`epap-emacs' to start an Emacs session."
    echo
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

    pkg-config
    openssl
    # freetype
    # harfbuzz

    (writeShellScriptBin "epap-shell" ''
      exec ${bashInteractive}/bin/bash --rcfile ${bashrc}
    '')
  ];

  EMACS_SITE_LISP = "${nodfur-emacs-packages.slime}/share/emacs/site-lisp";

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
