{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
    flake-utils.url = github:numtide/flake-utils;
    nodfur.url = github:nodfur/os;
    zig.url = github:arqv/zig-overlay;
  };

  nixConfig.bash-prompt =
    ''(epap) \[\e[1m\]\h\[\e[0m\]:\w\[\e[1m\]\[\e[0m\]\$ '';

  outputs = { self, nixpkgs, flake-utils, nodfur, zig }:
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

          zig = zig.packages."${system}"."0.9.0";
        };

        packages.epap-light = pkgs.callPackage ./epap-light.nix {
          inherit (nodfur-packages)
            nodfur-emacs
            nodfur-emacs-packages
          ;
        };

        devShell = packages.epap;
        devShells.epap-light = packages.epap-light;
      }
    );
}
