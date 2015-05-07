{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; pkgs.haskellPackages.mkDerivation {
  pname = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    pkgs.pkgconfig Cabal cabal-install ghc-core
    cassava hspec criterion #ghcid
    random alsa-core binary pkgs.sox
    jack pkgs.jack2 pkgs.qjackctl pkgs.a2jmidid pkgs.jackmeter
  ];
  license = pkgs.stdenv.lib.licenses.gpl3;
}
