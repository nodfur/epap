{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
  };

  outputs = { self, nixpkgs }:
    let 
      epap = system: (
        import nixpkgs { inherit system; }
      ).callPackage ./epap.nix {};
    in {
      packages.aarch64-linux.epap = epap "aarch64-linux";
      packages.x86_64-linux.epap = epap "x86_64-linux";
    };
}
