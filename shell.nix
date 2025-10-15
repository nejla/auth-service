# Using pinned nixpkgs.
#
# Updating:
#
# the nix directory is a symlink to Avancera's. This is to keep duplication down

let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};

    # has

in with pkgs;

stdenv.mkDerivation {
  name = "com.nejla.app";
  buildInputs= [
    gnumake
    pandoc
    nodejs
    nodePackages.npm
    openssl
    docker-compose
    curl
    jq
    websocat
    zlib
    zlib.dev
    pkg-config
    libxml2
    mpack
    haskell.compiler.ghc964

    (haskell-language-server.override
      {supportedGhcVersions = [ "964" ];
      } )


    postgresql


  ];

  # Without this, stack gets sad
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG="en_US.UTF-8";

}
