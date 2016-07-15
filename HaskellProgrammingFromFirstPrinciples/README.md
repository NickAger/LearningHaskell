# LearningHaskell
Exercises completed while working through http://haskellbook.com. The code is contained in 
workspaces made within http://haskellformac.com.

I would love to compare my exercises solutions with others learning haskell through [Haskell Programming from first principles](http://haskellbook.com) book. Please contact me.

----

Some links:
* Comprehensive Stack [overview video tutorial](https://www.youtube.com/watch?v=sRonIB8ZStw)
* http://lonelyproton.com/posts/26-getting-started-with-haskell-development-3
* https://github.com/bitemyapp/learnhaskell/blob/master/code_to_learn_from.md

From the stack video:
```
> docker run -t -i ubuntu bash
> ...install stack
> stack setup
> ls ~/.stack/programs/x86_64-osx/ghc-7.10.3
> stack path
> stack build wreq
>
> -- finding out where docker file system is on the local machine
> cd /var/lib/docker/aufs/mnt
> docker ps
> cd 
> cd ~/.stack/snapshots/x86_64-osx/lts-5.9/7.10.3/pkgdb
>
> stack install pandoc
>
> stack new test-project
```

"stack replaces `cabal install`"

```
sudo apt-get darcs
```

To take a project which doesn't have a `stack.yml` and add one:

```
stack init
```
this will try to find stackage lts that will resolve all dependencies. If however you want to specify an lts version manually:

```
stack init --resolver lts-3.10
```

finding out which debian package contains some libraries:

```
sudo apt-cache search icu dev
```

alternatively when you are searching for a specific shared object file (or DLL) eg `libtinfo.so`. First install `apt-file`

```
sudo apt-get install apt-file
sudo apt-file update
sudo apt-file search libtinfo.so
sudo apt-get intall libtinfo-dev
```


---

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
