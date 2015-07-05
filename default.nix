{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  pkgs = nixpkgs.pkgs.haskell.packages.${compiler};
  biegunka =  pkgs.callPackage ../biegunka/biegunka.nix {
    mkDerivation = args: pkgs.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.git ];
    });
  };
in
  pkgs.callPackage ./package.nix {
    mkDerivation = args: pkgs.mkDerivation(args // {
      buildTools = (if args ? buildTools then args.buildTools else []) ++ [ nixpkgs.pkgs.subversion ];
    });
    biegunka = biegunka;
  }
