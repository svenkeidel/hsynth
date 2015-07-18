{ pkgs ? import <nixpkgs> {} }:

let lib = pkgs.haskell-ng.lib;
  haskellPackages' = pkgs.haskellPackages.override {
     overrides = self: super: {
       vector = super.vector.override {
         mkDerivation = (attrs: self.mkDerivation (attrs // { doCheck = false; }));
       };
     };
   };
  env = haskellPackages'.ghcWithPackages(p: with p; [
    Cabal cabal-install ghc-core cassava hspec criterion
    random alsa-core explicit-exception pulse-simple
    binary linear jack plot plot-gtk hmatrix dbus
  ]);
in pkgs.stdenv.mkDerivation {
  name = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ pkgs.pkgconfig pkgs.alsaLib pkgs.SDL2 pkgs.jack2 pkgs.dfeet env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
