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
    rm -rf build result
  '';

  buildPhase = ''
    mkdir -p build
    ghc --make \
      -isrc \
      -O2 \
      -outputdir build \
      -o build/collage \
      src/Main.hs

    ${pkgs.help2man}/bin/help2man build/collage \
      --version-string "${version}" \
      > build/collage.1
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

    installManPage build/collage.1

    # Completion scripts need final binary path
    # and must be built after `installPhase`.
    $out/bin/collage --bash-completion-script $out/bin/collage \
      > build/collage.bash
    $out/bin/collage --fish-completion-script $out/bin/collage \
      > build/collage.fish
    $out/bin/collage --zsh-completion-script $out/bin/collage \
      > build/collage.zsh

    installShellCompletion build/collage.{bash,fish,zsh}
  '';

  shellHook = ''
    export PATH=\
    ${pkgs.haskellPackages.haskell-language-server}/bin:\
    $PATH
  '';
}
