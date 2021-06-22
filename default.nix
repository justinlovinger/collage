{ pkgs ? (import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels.git";
  ref = "nixos-unstable";
  rev = "1e3f09feaa5667be4ed6eca96a984b4642420b83";
}) {}) }:

pkgs.stdenv.mkDerivation rec {
  pname = "collage";
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
      -o build/${pname} \
      src/Main.hs

    ${pkgs.help2man}/bin/help2man build/${pname} \
      --version-string "${version}" \
      > build/${pname}.1
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
    mv build/${pname} $out/bin/

    installManPage build/${pname}.1

    # Completion scripts need final binary path
    # and must be built after `installPhase`.
    $out/bin/${pname} --bash-completion-script $out/bin/${pname} \
      > build/${pname}.bash
    $out/bin/${pname} --fish-completion-script $out/bin/${pname} \
      > build/${pname}.fish
    $out/bin/${pname} --zsh-completion-script $out/bin/${pname} \
      > build/${pname}.zsh

    installShellCompletion build/${pname}.{bash,fish,zsh}
  '';

  shellHook = ''
    export PATH=\
    ${pkgs.haskellPackages.haskell-language-server}/bin:\
    $PATH
  '';

  meta = with pkgs.lib; {
    description = "Create a collage of semi-random images";
    homepage = "https://github.com/JustinLovinger/collage";
    license = licenses.mit;
    maintainers = [ ];
  };
}
