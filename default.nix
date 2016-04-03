{ pkgs ? import <nixpkgs> {} }:

let env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint mtl cassava binary vector text unix
    hspec plot plot-gtk hmatrix criterion
    #llvm-general llvm-general-pure
  ]);
in pkgs.stdenv.mkDerivation {
  name = "dsp-dsl";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ pkgs.pkgconfig pkgs.libjack2 env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    export NIX_GHC_LIBDOCDIR="${env}/share/doc/x86*/"
  '';
}
