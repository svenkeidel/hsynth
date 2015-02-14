{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; cabal.mkDerivation (self: {
  pname = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    cabalInstall random alsaCore
    pkgs.sox pkgs.pkgconfig
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
