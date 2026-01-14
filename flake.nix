{
  description = "jrrom.com dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };

  outputs = { self , nixpkgs ,... }:
    let
      system = "x86_64-linux";
    in
      {
        devShells."${system}".default = let
          pkgs = import nixpkgs { inherit system; };
        in pkgs.mkShell {
          # Create an environment with Racket
          packages = with pkgs; [
            (ghc.withPackages(ps: with ps; [
              cabal-install
              haskell-language-server
            ]))
            zlib
            ormolu
            dart-sass
          ];

          shellHook = ''
          export PATH=$PATH:~/.local/bin
          '';
        };
      };

}

