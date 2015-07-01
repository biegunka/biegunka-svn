{ mkDerivation, base, biegunka, directory, process, stdenv
, transformers
}:
mkDerivation {
  pname = "biegunka-svn";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ base biegunka directory process transformers ];
  homepage = "https://github.com/biegunka/biegunka-svn";
  description = "SVN support for Biegunka";
  license = stdenv.lib.licenses.bsd2;
}
