{ mkDerivation, base, biegunka, directory, filepath
, hspec-expectations-lens, process, stdenv, temporary, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ base biegunka directory process transformers ];
  testDepends = [
    base directory filepath hspec-expectations-lens process temporary
    transformers
  ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
