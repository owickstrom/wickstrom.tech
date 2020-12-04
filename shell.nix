{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = [
    pkgs.awscli
    pkgs.goaccess

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
