{ stdenv, sbcl, lispPackages }:

stdenv.mkDerivation {
  name = "epap-sbcl";
  version = "0.5";
  src = ./.;
  buildInputs = [
    sbcl
    lispPackages.cffi
    lispPackages.alexandria
    lispPackages.trivial-features
    lispPackages.babel
  ];
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${lispPackages.clwrapper}/bin/common-lisp.sh $out/bin/epap-sbcl
  '';
}
