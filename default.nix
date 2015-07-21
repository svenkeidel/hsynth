{ pkgs ? import <nixpkgs> {} }:

let env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install ghc-core cassava hspec criterion
    random alsa-core explicit-exception pulse-simple
    binary linear plot plot-gtk hmatrix
  ]);
in pkgs.stdenv.mkDerivation {
  name = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ pkgs.pkgconfig pkgs.alsaLib pkgs.SDL2 pkgs.libjack2 pkgs.dfeet env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
