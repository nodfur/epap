{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
    flake-utils.url = github:numtide/flake-utils;
    nodfur.url = github:nodfur/os;
  };

  outputs = { self, nixpkgs, flake-utils, nodfur }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        nodfur-packages = nodfur.packages."${system}";
      in rec {
        packages.epap = pkgs.callPackage ./epap.nix {
          inherit (nodfur-packages)
            nodfur-emacs
            nodfur-emacs-packages
            restless-git
          ;
        };

        devShell = packages.epap;
      }
    );
}
