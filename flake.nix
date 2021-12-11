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
        packages.epap =
          pkgs.callPackage ./epap.nix {
            zig = if system == "aarch64-darwin"
                  then null else pkgs.zig;
          };
        
        devShell = packages.epap;
      }
    );
}
