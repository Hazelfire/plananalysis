{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./default.nix {};
  ghcide = import (
    pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "ghcide-nix";
      rev = "caab5c37a80c4b706ebdc5b50ad610cc96d6b9e2";
      sha256 = "0vyvjv82q9w6fy51a7vmyva38qca7b2q67yznbsgi8ma64g5aqsa";
    }) {};

in
pkgs.mkShell {
  name = "shell";
  shellHook = ''
#    export NIX_GHC="$(which ghc)"
#    export NIX_GHCPKGS="$(which ghc-pkg)"
    export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
  '';
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.hpack
    haskellPackages.cabal2nix
    ghcide.ghcide-ghc865
    pkgs.dhall
  ];
}
