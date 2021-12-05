{ stdenv, sbcl, lispPackages }:

stdenv.mkDerivation {
  name = "epap-sbcl";
  version = "0.5";
  src = ./.;
  buildInputs = [sbcl] ++ with lispPackages; [
    cffi
    alexandria
    trivial-features
    babel
    zpng
    cl-base64
  ];
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${lispPackages.clwrapper}/bin/common-lisp.sh $out/bin/epap-sbcl
  '';
}
