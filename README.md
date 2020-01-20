# Haskell examples

This repository has multiple Haskell example programs from different sources
including Haskell books, videos, blog posts and other tutorials.

## Program in Haskell

The book [Programming in Haskell by Graham Hutton][100] is one of the best
resources in learning Haskell.

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

## Haskell and vim

To use [vim with Haskel][210] the author suggests installing

* [haskell-ide-engine][220]
* [hlint][230] - Haskell linter
* [hindent][240] - Haskell pretty printer

[210]: http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/
[220]: https://github.com/haskell/haskell-ide-engine/#installation-with-nix
[230]: https://github.com/ndmitchell/hlint
[240]: https://github.com/chrisdone/hindent

## Nix and Haskell

The [Nix and Haskell][300] repo demonstrates how to use Nix to build Haskell projects.

[300]: https://github.com/Gabriel439/haskell-nix
[310]: https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html

1. Setup the environment using Nix

```
nix-env -i cabal-install cabal2nix nix-prefetch-git ghc
```

2.

```
nix-env -i hlint
```

## Functors, applicatives and monads

[Blog post][400] demonstrating the same functionality using functors
applicatives and monads

[400]: https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads

## FPComplete

### Get started with Haskell

https://tech.fpcomplete.com/haskell/get-started

## Miscellaneous

### Experimenting in Haskell tips

1. Use -fdefer-type-errors in ghci so errors become warnings when experimenting

## Haskell operator names

Some [common names][500] used for Haskell operators are:

[500]: https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

```
!!  "index"
$
*>   "then"
++   "concat"
->   "to"
.    "pipe to" / "compose" / "dot"
:    "cons"
::   "has type" / "of type" / "is of type"
<$   "map-replace by"
<$>  "fmap" / "dollar cyclops"
<*>  "apply" / "star cyclops"
<-   "is drawn from"
<=<  "left fish"
<|>  "or"
=    "is defined to be" / "is defined as"
=>   "implies" / "then"
>>      then
>>=     bind
@    "as"
[]   "empty list"
\    "lambda"
_    "whatever"
|    "such that"
~    "lazy"
```

## Links

### Learning Haskell

[Haskell walk through for beginners][1000]

[1000]: http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html

[Introduction to Haskell][1010] from University of Penn

[1010]: https://www.seas.upenn.edu/~cis194/fall16/index.html

[Haskell course][1020]

[1020]: https://github.com/data61/fp-course

[Haskell learning advice][1030]

[1030]: http://mechanical-elephant.com/thoughts/2015-04-20-becoming-productive-in-haskell/index.html

[Google Haskell training][1040]

[1040]: https://github.com/google/haskell-trainings

### Good Haskell

[Haskell for all][1100]

[1100]: http://www.haskellforall.com/2015/09/how-to-make-your-haskell-code-more.html

[Parse, don't validate][1110]

[1110]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

[Haskell vs Python: Working with Trees][1120]

[1120]: https://doisinkidney.com/posts/2019-10-02-what-is-good-about-haskell.html

[Business rules][1130] in Haskell, F# and C#

[1130]: https://blog.ndcconferences.com/composite-as-a-monoid-a-business-rules-example/

[What I wish I knew when learning Haskell][1140]

[1140]: https://github.com/sdiehl/wiwinwlh

[Introduction to IO][1150]

[1150]: https://wiki.haskell.org/Introduction_to_IO

[Monads as containers][1160]

[1160]: https://wiki.haskell.org/Monads_as_containers

[Monads as computation][1170]

[1170]: https://wiki.haskell.org/Monads_as_computation

### Haskell libraries

[Text to structured data][1210]

[1210]: https://github.com/facebook/duckling

[RIO: a standard library for Haskell][1220]

[1220]: https://github.com/commercialhaskell/rio

Haskell [shell scripting][1230]

[1230]: https://github.com/Gabriel439/Haskell-Turtle-Library

### Miscellaneous

[Why not Haskell][1300]

[1300]: https://pchiusano.github.io/2017-01-20/why-not-haskell.html

[Why Haskell][1310] for Github Semantic

[1310]: https://github.com/github/semantic/blob/master/docs/why-haskell.md

