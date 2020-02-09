{ pkgs ? import <nixpkgs> {} }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          plananalysis = pkgs.haskellPackages.callPackage ./nix/plananalysis.nix {};
        };
      };
    };
  };
  myPkgs = import <nixpkgs> {inherit config;};
in
  myPkgs.haskellPackages.plananalysis

