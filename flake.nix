{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
  };

  outputs = { self, nixpkgs }:
    let
      epap = system: (
        import nixpkgs { inherit system; }
      ).callPackage ./epap.nix {};

      epap-sbcl = system: (
        import nixpkgs { inherit system; }
      ).callPackage ./epap-sbcl.nix {};
    in {
      packages.aarch64-linux.epap = epap "aarch64-linux";
      packages.aarch64-darwin.epap = epap "aarch64-darwin";
      packages.x86_64-linux.epap = epap "x86_64-linux";

      packages.aarch64-darwin.epap-sbcl = epap-sbcl "aarch64-darwin";
      packages.aarch64-linux.epap-sbcl = epap-sbcl "aarch64-darwin";

      devShell.x86_64-linux = epap "x86_64-linux";
      devShell.aarch64-linux = epap "aarch64-linux";
    };
}
