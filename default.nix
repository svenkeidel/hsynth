{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; cabal.mkDerivation (self: {
  pname = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    cabalInstall pkgs.pkgconfig ghcCore
    cassava hspec criterion
    random alsaCore binary pkgs.sox
    jack pkgs.jack2 pkgs.qjackctl pkgs.a2jmidid pkgs.jackmeter
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
