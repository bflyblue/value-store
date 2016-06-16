{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103", devtools ? nixpkgs.config.devtools }:
let env = (import ./default.nix { inherit nixpkgs compiler; }).env;
    hspkg = nixpkgs.pkgs.haskell.packages.${compiler};
in
    nixpkgs.pkgs.lib.overrideDerivation env (x: {
      buildInputs = x.buildInputs ++ devtools (hspkg);
    })
