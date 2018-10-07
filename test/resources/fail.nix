# A failing nix-script
let
  pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "fail";
  phases = "installPhase";
  inputs = [];
  installPhase = ''
    echo 'Some nonsense' >$out
    echo "Bad information"
    exit -1
  '';
}
