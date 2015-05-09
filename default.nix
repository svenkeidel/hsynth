{ pkgs ? import <nixpkgs> {} }:

let pkg = pkgs.haskellPackages.callPackage (
  { mkDerivation, base, ghc, Cabal, cabal-install, ghc-core, cassava, hspec,
    criterion, random, alsa-core, binary, jack }:
    mkDerivation {
      pname = "hsynth";
      version = "0.1.0.0";
      src = ./.;
      buildDepends = [
        pkgs.pkgconfig Cabal base cabal-install ghc-core cassava hspec criterion
        random alsa-core binary pkgs.sox jack pkgs.jack2 pkgs.qjackctl
        pkgs.a2jmidid pkgs.jackmeter
      ];
      license = pkgs.stdenv.lib.licenses.gpl3;
    }) {};
in pkg.env
