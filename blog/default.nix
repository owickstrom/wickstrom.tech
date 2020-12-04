{ pkgs ? import <nixpkgs> { } }:
let
  gems = pkgs.bundlerEnv {
    name = "wickstrom-tech-gems";
    ruby = pkgs.ruby;
    gemdir = ./src;
  };

  filters = import ../filters { inherit pkgs; };

  pandoc-include-code = let
    prefetched =
      builtins.fromJSON (builtins.readFile ./nix/pandoc-include-code.json);
    src = pkgs.fetchFromGitHub {
      owner = "owickstrom";
      repo = "pandoc-include-code";
      inherit (prefetched) rev sha256;
    };
  in import src { };

in pkgs.stdenv.mkDerivation {
  name = "wickstrom-tech-blog";
  src = ./.;
  buildInputs = [
    gems
    filters
    pandoc-include-code

    pkgs.glibcLocales
    pkgs.ruby
    pkgs.pandoc
    pkgs.plantuml
  ];

  buildPhase = ''
    export LC_ALL="en_US.UTF-8"
    make all
  '';

  installPhase = ''
    cp -r target/html $out
  '';
}
