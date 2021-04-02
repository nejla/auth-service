with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "signed-authorization";
  buildInputs = [ stack
                  gnumake
                  openssl
                ];
}
