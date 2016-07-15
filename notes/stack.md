* Comprehensive Stack [overview video tutorial](https://www.youtube.com/watch?v=sRonIB8ZStw)
* http://lonelyproton.com/posts/26-getting-started-with-haskell-development-3

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
