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
    random alsa-core binary explicit-exception pulse-simple
    sdl2
  ]);
in pkgs.stdenv.mkDerivation {
  name = "hsynth";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ pkgs.pkgconfig pkgs.alsaLib env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
