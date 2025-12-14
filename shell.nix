let
  pkgs = (import ./default.nix).pkgs;
in
pkgs.stdenv.mkDerivation {
	name = "env";
	buildInputs = [ pkgs.haskellPackages.haskell-language-server ]
      ++ [ pkgs.haskellPackages.cabal-install]
      ++ [ pkgs.mesa_glu pkgs.freeglut ];
}
