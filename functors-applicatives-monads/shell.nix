
{ pkgs ? import <nixpkgs> {} }:
let 
  # From these instructions
  # https://github.com/Infinisil/all-hies
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  haskell-deps = deps: with deps; [
    base
    hlint
    hindent
    hoogle
  ];
  my-haskell-packages =  pkgs.haskellPackages.ghcWithPackages haskell-deps;
  my-haskell = [
    # (all-hies.unstableFallback.selection { selector = p: { inherit (p) ghc865; }; })
    my-haskell-packages
  ];
in pkgs.mkShell {
  buildInputs = [
    my-haskell
  ];
}
