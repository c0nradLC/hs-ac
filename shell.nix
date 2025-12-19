let
  pkgs = (import ./default.nix).pkgs;
in
pkgs.haskellPackages.shellFor {
  packages = p: [ 
    (p.callCabal2nix "hs-ac" ./. {})
  ];

  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.OpenGL
  ];

  nativeBuildInputs = [
    pkgs.mesa_glu
    pkgs.libGL
    pkgs.libGLU
    pkgs.freeglut
  ];
}
