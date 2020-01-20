{ pkgs ? import <nixpkgs> {} }:
let 
  haskell-deps = deps: with deps; [
    text
    safe
    hlint
    hindent
  ];
  # my-haskell-packages =  pkgs.haskellPackages.ghcWithPackages haskell-deps;
  my-haskell-packages =  pkgs.haskellPackages.ghcWithHoogle haskell-deps;
  my-haskell = [
    my-haskell-packages
    pkgs.ghcid
  ];
in pkgs.mkShell {
  buildInputs = [
    my-haskell
  ];
}
