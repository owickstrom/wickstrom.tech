{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; } }:
let
  gems = pkgs.bundlerEnv {
    name = "wickstrom-tech-gems";
    ruby = pkgs.ruby;
    gemdir = ./src;
  };

  fonts = with pkgs; [ libertine ];

  haskellPackages = pkgs.haskell.packages.ghc884.override {
    overrides = self: super: {
      active =
        pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck super.active);
      monoid-extras = pkgs.haskell.lib.doJailbreak super.monoid-extras;
      dual-tree = pkgs.haskell.lib.doJailbreak super.dual-tree;
    };
  };

  blog-diagrams = import ../diagrams { inherit pkgs haskellPackages; };

  haskellEnv = (haskellPackages.ghcWithPackages
    (p: [ p.diagrams-lib p.diagrams-cairo p.diagrams-svg blog-diagrams ]));

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
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  buildInputs = [
    gems
    filters
    pandoc-include-code

    pkgs.glibcLocales
    pkgs.ruby
    pkgs.pandoc
    pkgs.plantuml

    haskellEnv
  ];

  buildPhase = ''
    export LC_ALL="en_US.UTF-8"
    make all
  '';

  installPhase = ''
    cp -r target/html $out
  '';

  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = fonts; };
}
