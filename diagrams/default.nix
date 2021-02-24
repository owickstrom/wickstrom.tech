{ pkgs ? import ../nixpkgs.nix { }, haskellPackages }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in haskellPackages.callCabal2nix "diagrams" "${src}" { }
