# Haskell programming

## Program in Haskell

This repository is used to work through the book [Programming in Haskell by
Graham Hutton][100]

[100]: https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229/

### Setup the environment

1. Follow the instructions in the "Nix and Haskell" section and it will install
   ghci. It does not install stack.

2. Start ghci


#### Install Stack

[Stack][200] is a cross-platform tool for developing Haskell projects.

[200]: https://docs.haskellstack.org/en/stable/README/

1. Install Stack
curl -sSL https://get.haskellstack.org/ | sh

## Nix and Haskell

The [Nix and Haskell][300] repo demonstrates how to use Nix to build Haskell projects.

[300]: https://github.com/Gabriel439/haskell-nix

1. Setup the environment using Nix

```
nix-env -i cabal-install cabal2nix nix-prefetch-git ghc
```

2. Clone the project

```
git clone https://github.com/Gabriel439/haskell-nix
```

3. Change to the code directory

```
cd ./haskell-nix
```

4. Change to the correct project

```
cd project0
cd project1
cd project2
cd project3
cd project4
```

## FPComplete

### Get started with Haskell

https://tech.fpcomplete.com/haskell/get-started


## Links

[Haskell walk through for beginners][1000]

[1000]: http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html

[Introduction to Haskell][1010] from University of Penn

[1010]: https://www.seas.upenn.edu/~cis194/fall16/index.html

[Haskell course][1020]

[1020]: https://github.com/data61/fp-course
