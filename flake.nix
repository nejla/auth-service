{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      ghcVersion = "967";
      haskell= pkgs.haskell.packages."ghc${ghcVersion}";
      # Wrap Stack to work with our Nix integration. We don't want to modify
      # stack.yaml so non-Nix users don't notice anything.
      # - no-nix: We don't want Stack's way of integrating Nix.
      # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      stack-wrapped = pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [ pkgs.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };
      devTools = [
          haskell.cabal-install
          haskell.ghc
          haskell.haskell-language-server # LSP server for editor
          haskell.hpack # LSP server for editor
          haskell.fourmolu # Haskell formatter
          pkgs.curl
          pkgs.docker-compose
          pkgs.gnumake
          pkgs.go-task
          pkgs.jq
          pkgs.libxml2
          pkgs.mpack
          pkgs.nodePackages.npm
          pkgs.nodejs
          pkgs.openssl
          pkgs.pandoc
          pkgs.pkg-config
          pkgs.postgresql_17
          pkgs.postgresql_17.pg_config
          pkgs.websocat
          pkgs.zlib
          pkgs.zlib.dev
          stack-wrapped
        ];


    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = devTools;

        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LANG = "en_US.UTF-8";
      };
    };
}
