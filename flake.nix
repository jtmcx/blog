{
  description = "Static site generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;

        ssg = haskellPackages.callCabal2nix "ssg" ./. { };
      in
      {
        packages.default = ssg;

        devShells.default = haskellPackages.shellFor {
          packages = _: [ ssg ];
          nativeBuildInputs = [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ormolu
            pkgs.cabal2nix
            pkgs.just
          ];
        };
      });
}
