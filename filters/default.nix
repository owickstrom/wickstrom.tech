{ pkgs ? import ../nixpkgs.nix { } }:
let
  haskellPackages = pkgs.haskell.packages.ghc884;
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in haskellPackages.callCabal2nix "filters" "${src}" { }
