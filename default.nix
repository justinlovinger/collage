{ pkgs ? (import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels.git";
  ref = "nixos-unstable";
  rev = "1e3f09feaa5667be4ed6eca96a984b4642420b83";
}) {}) }:

pkgs.stdenv.mkDerivation {
  pname = "haskell-pbil";
  version = "1.0.0";

  src = ./.;

  nativeBuildInputs = [ (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    hip
    random
  ])) ];

  configurePhase = ''
    rm -rf build result
  '';

  buildPhase = ''
    mkdir -p build
    ghc --make \
      -isrc \
      -O2 \
      -outputdir build \
      -o build/haskell-pbil \
      src/Main.hs
  '';

  # doCheck = true;
  checkPhase = ''
    mkdir -p build
    ghc --make \
      -isrc \
      -itest \
      -outputdir build \
      -o build/runtests \
      test/Main.hs \
    && ./build/runtests
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv build/haskell-pbil $out/bin/
  '';

  shellHook = ''
    export PATH=\
    ${pkgs.haskellPackages.haskell-language-server}/bin:\
    $PATH
  '';
}
