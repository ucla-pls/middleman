let
  pkgs = import <nixpkgs> {};
in builtins.map (i: pkgs.stdenv.mkDerivation {
  name = "hello-${i}";
  phases = "installPhase";
  inputs = with pkgs; [ python jq httpie ];
  installPhase = ''
    sleep 23
    echo 'hello-${i}' >$out
  '';
}) ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10"]
