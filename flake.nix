{
  inputs = {
    nixpkgs.url = github:nodfur/nixpkgs/nodfur;
  };

  outputs = { self, nixpkgs }: {
    packages.aarch64-linux.epap =
      let 
        pkgs = import nixpkgs { 
          system = "aarch64-linux"; 
        };
      in pkgs.callPackage ./epap.nix {};
  };
}