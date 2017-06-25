# This doesn't work, it is broken and needs some more work to be fixed.

# Generated using stack2nix 0.1.2.0.
#
# Only works with sufficiently recent nixpkgs, e.g. "NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz".

{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc802
, ghc ? pkgs.haskell.compiler.ghc802
}:

with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  stackPackages = { callPackage, pkgs, stdenv }:
self: {
      hiper = callPackage ({ base, bytestring, containers, directory, mkDerivation, mtl, parsec, scientific, stdenv, text, unordered-containers, vector, yaml }:
      mkDerivation {
          pname = "hiper";
          version = "0.1.0.0";
          src = ./.;
          libraryHaskellDepends = [
            base
            bytestring
            containers
            directory
            mtl
            parsec
            scientific
            text
            unordered-containers
            vector
            yaml
          ];
          doHaddock = false;
          doCheck = false;
          homepage = "https://github.com/githubuser/hiper#readme";
          license = stdenv.lib.licenses.bsd3;
        }) {};
    };
in
compiler.override {
  initialPackages = stackPackages;
}
