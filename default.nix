{ pkgs ? (import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels.git";
  ref = "nixos-unstable";
  rev = "1e3f09feaa5667be4ed6eca96a984b4642420b83";
}) {}) }:

pkgs.stdenv.mkDerivation rec {
  pname = "random-collage";
  version = "1.0.0";

  src = ./.;

  outputs = [ "out" "man" ];

  nativeBuildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      hip
      optparse-applicative
      random
    ]))
    pkgs.installShellFiles
  ];

  configurePhase = ''
    rm -rf build doc result
  '';

  buildPhase = ''
    mkdir -p build
    ghc --make \
      -isrc \
      -O2 \
      -outputdir build \
      -o build/collage \
      src/Main.hs

    mkdir -p doc
    ${pkgs.help2man}/bin/help2man build/collage \
      --version-string "${version}" \
      > doc/collage.1
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
    mv build/collage $out/bin/

    installManPage doc/collage.1
  '';

  shellHook = ''
    export PATH=\
    ${pkgs.haskellPackages.haskell-language-server}/bin:\
    $PATH
  '';
}
