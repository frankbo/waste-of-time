with (import <nixpkgs> {});

stdenv.lib.makeOverridable stdenv.mkDerivation {
  name = "waste-of-time";
  version = "1.0";

  buildInputs = [ elmPackages.elm elmPackages.elm-format nodejs-8_x ];

  buildPhase = ''
   '';

   installPhase = ''
   '';

   shellHook = ''
     mkdir -p $out/bin
     npm install create-elm-app
     cp -R ./node_modules/.bin $out/bin

     export PATH="$PWD/node_modules/.bin/:$PATH"
   '';
}