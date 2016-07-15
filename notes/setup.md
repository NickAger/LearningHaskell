From the getting started with haskell:

```bash
$ stack install hdevtools
$ stack install pointfree
$ stack install hlint
```
* Hdevtools delivers super fast type checking.
* Pointfree allows you get to quickly convert functions to pointfree form. I find is a great way to learn more about how to compose functions. Don't overdo it though.
* hlint is the best linting tool I've seen for any language.

using stack version of ghc and ghci via command aliases
```
alias ghc='stack exec -- ghc'
alias ghci='stack exec -- ghci'
```
