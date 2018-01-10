{ mkDerivation, base, comonad, lens, profunctors, stdenv }:
mkDerivation {
  pname = "2d-zipper";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base comonad lens profunctors ];
  license = stdenv.lib.licenses.bsd3;
}
