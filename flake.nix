{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {
        packages.epap = pkgs.callPackage ./epap.nix {};
        devShell = packages.epap;
      }
    );
}
