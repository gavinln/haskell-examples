with import <nixpkgs> {};

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    # other python packages you want
  ]; 
  python-with-my-packages = python37.withPackages my-python-packages;
  my-general-packages = [ git ];
  my-go-packages = [ go ];
  my-rust-packages = [ cargo ];
  my-c-packages = [ gcc ];
in mkShell {
  buildInputs = [
    my-general-packages
    my-go-packages
    my-rust-packages
    my-c-packages
    # python-with-my-packages
  ];
  shellHook = ''
    # alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' \pip"
    # export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python3.7/site-packages:$PYTHONPATH"
    export GOPATH="$(pwd)/go-lang"
    unset SOURCE_DATE_EPOCH
  '';
}
