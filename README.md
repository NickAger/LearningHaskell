# LearningHaskell
Learning Haskell by studying:

* [Haskell Programming from first principles](http://haskellbook.com) - [@haskellbook](https://twitter.com/haskellbook) - my main learning resource
* [Penn University CIS 194](http://www.seas.upenn.edu/~cis194/lectures/01-intro.html) - I completed the [exercises](https://github.com/NickAger/LearningHaskell/tree/master/CIS194) for first 4 lectures.  
* [Well-Typed Haskell training](http://www.well-typed.com/services_training/)

The majority of the work is in: [HaskellProgrammingFromFirstPrinciples](https://github.com/NickAger/LearningHaskell/tree/master/HaskellProgrammingFromFirstPrinciples). I would love to compare my exercises solutions with others learning haskell through [Haskell Programming from first principles](http://haskellbook.com) book. Please contact me.

Workspaces made within [Haskell for Mac](http://haskellformac.com) and code outside of [Haskell for Mac](http://haskellformac.com) playgrounds was developed using
[Atom configured for Haskell](http://achernyak.me/universal-haskell-dev-enviornment) and then laterly using [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=Vans.haskero).

## My Notes

* [newtype](notes/newtype.md)
* [Orphan instances](notes/orphaninstances.md)
* [NonEmpty](notes/NonEmpty.md)
* [fmap](notes/fmap.md)
* [stack](notes/stack.md)
* [setup](notes/setup.md)
* [CoArbitrary](notes/CoArbitrary.md)
* [composition](notes/composition.md)
* [lifting](notes/lifting.md)

# Links

* [Monads and all that  - John Hughes](https://www.youtube.com/watch?v=w_KY2I34-f8) - excellent series of lectures.
* [MagicHaskeller](http://nautilus.cs.miyazaki-u.ac.jp/~skata/MagicHaskeller.html) - specify parameters and output and it will find the function composition for you eg: `f "hello" == "HELLO"` -> `f = map toUpper`
* [Haskell CheatSheet PDF](http://cheatsheet.codeslower.com/CheatSheet.pdf)
* [Functors, Applicatives and Monads in pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
* [You Could Have Invented Monads! (And Maybe You Already Have.)](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
* [Open-source Haskell projects](https://github.com/bitemyapp/learnhaskell/blob/master/code_to_learn_from.md) - "Code to learn from"
* [Teaching Haskell for Understanding](https://docs.google.com/presentation/d/1_I5mYXivG5NbOHnICld_Xo41uOyAT57Yade5qfz2toQ/edit#slide=id.g226ee758c3_0_284)
* [The five arguments on why people struggle with monads](http://chrisdone.com/posts/monads)
* [A wreq tutorial](http://www.serpentine.com/wreq/tutorial.html) - Learn how to write web clients. "We start easy, then ramp up the power."
* [Announcing: Snap 1.0](https://www.reddit.com/r/haskell/comments/4wody6/announcing_snap_10/)
* [A collection of interesting Haskell talks](https://github.com/0xmohit/talks)
* [A monad for reactive programming](https://www.schoolofhaskell.com/user/agocorona/monad-reactive-programming-2)
* [Monads: From Web 2.0 to Hardware Drivers](http://www.well-typed.com/blog/2015/02/ziria/)
* [Dependently typed servers in Servant](http://www.well-typed.com/blog/2015/12/dependently-typed-servers/)
* [Generative Art Haskell](https://github.com/rickerbh/GenerativeArtHaskell) - Playground for Haskell For Mac
* [Why Haskell is Great - 10 minutes](https://www.youtube.com/watch?v=RqvCNb7fKsg)
* [Four months with Haskell](http://lexi-lambda.github.io/blog/2016/06/12/four-months-with-haskell/)
* [The Evolution of a Haskell Programmer](http://www.willamette.edu/~fruehr/haskell/evolution.html)
* [Haskell 'sequence' Over Functions - Explained](http://derekwyatt.org/2012/01/25/haskell-sequence-over-functions-explained/)
* [The Interpreter Pattern Revisited](https://www.youtube.com/watch?v=hmX2s3pe_qk) idea of algebraic types as simple languages "The Interpreter Pattern is the only pattern from the GOF book worth considering"
* [Rust as a gateway drug to Haskell](https://news.ycombinator.com/item?id=14550606) - article contains lots of interesting comparisons between Haskell and Rust and the Hacker News comments have lots of interesting links to Haskell background and extensions.

# Beyond Functors
* [Phil Freeman - Fun with Profunctors](https://www.youtube.com/watch?v=OJtGECfksds)
* [George Wilson - The Extended Functor Family](https://www.youtube.com/watch?v=JZPXzJ5tp9w)

# Free Monad

> Free monads let you decompose any impure program into a pure representation of its behavior and a minimal impure interpreter.
Source: [Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)

* [No Silver Bullets in Functional Programming by Brian McKenna at Functional Conf 16](https://www.youtube.com/watch?v=UfowUAjQC3Y) - about referential transparency and how to add it imperative programs; hint it use Free Monads
* [Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
* [Free Monads Are Simple](https://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html) - example using Haxl.
* [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)
* [Purity in an impure language with the free monad – by example of a Tic-Tac-Toe backend with CQRS and event sourcing](http://blog.leifbattermann.de/2016/12/25/purity-in-an-impure-language-free-monad-tic-tac-toe-cqrs-event-souring/)
* [Way 12. Monadic control flow, in which we make decisions in the turtle workflow based on results from earlier commands.](https://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle-2/#way13) from [Thirteen ways of looking at a turtle](https://fsharpforfunandprofit.com/turtle/)
* [Free for DSLs, cofree for interpreters](http://dlaing.org/cofun/posts/free_and_cofree.html)
* [Free monad cheetsheet](http://jeremymikkola.com/posts/2017_07_11_free_monad_cheatsheet.html)
* [Free monads in 7 easy steps](http://joashc.github.io/posts/2015-09-13-free-monad-steps.html)
* [Free monads in category theory](http://joashc.github.io/posts/2016-03-23-free-monads.html)
* [What is referential transparency](https://stackoverflow.com/questions/210835/what-is-referential-transparency/11740176#11740176)

# Laziness
* [More points for lazy evaluation](http://augustss.blogspot.co.uk/2011/05/more-points-for-lazy-evaluation-in.html)
* https://www.reddit.com/r/haskell/comments/5xge0v/today_i_used_laziness_for/
* http://stackoverflow.com/questions/23893320/why-isnt-this-recursive-function-being-optimized-haskell/23893575#23893575
> Philip Walder:
> With distribution becoming so important, we need to focus on programs that run on multiple machines, sending values from one to the other. When you send a value, you probably want it to be the value itself (eager evaluation), rather than a program (and the values of all the free variables of the program) that can be evaluated to yield the value. So, in the distributed world, I think it would be better to be eager by default but make it easy to be lazy when you want. 

> lazy in the spine, strict in the leaves.

# Category theory
* [Category Theory for the Working Hacker by Philip Wadler](https://www.youtube.com/watch?v=V10hzjgoklA)
* [Dr Eugenia Cheng Category Theory videos](https://www.youtube.com/watch?v=yeQcmxM2e5I&list=PLlGXNwjYhXYxKVa67r0pKuYufECy713bv)
* [Dr Eugenia Cheng Category Theory lecture notes](http://cheng.staff.shef.ac.uk/catnotes/categorynotes-cheng.pdf)

