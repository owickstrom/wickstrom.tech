{ pkgs ? import <nixpkgs> { } }:
let

  haskellPackages = pkgs.haskell.packages.ghc884;

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  drv = haskellPackages.callCabal2nix "wickstrom-tech" "${src}" { };

in haskellPackages.shellFor {
  withHoogle = true;
  packages = (p: [drv]);
  buildInputs = [
    pkgs.cabal-install
    haskellPackages.pandoc

    haskellPackages.haskell-language-server
    haskellPackages.ormolu

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
