# LearningHaskell
Learning Haskell by studying:

* [Haskell Programming from first principles](http://haskellbook.com) - [@haskellbook](https://twitter.com/haskellbook) - my main learning resource
* [Penn University CIS 194](http://www.seas.upenn.edu/~cis194/lectures/01-intro.html) - I completed the [exercises](https://github.com/NickAger/LearningHaskell/tree/master/CIS194) for first 4 lectures.  
* [Well-Typed Haskell training](http://www.well-typed.com/services_training/)

The majority of the work is in: [HaskellProgrammingFromFirstPrinciples](https://github.com/NickAger/LearningHaskell/tree/master/HaskellProgrammingFromFirstPrinciples). I would love to compare my exercises solutions with others learning haskell through [Haskell Programming from first principles](http://haskellbook.com) book. Please contact me.

Workspaces made within [Haskell for Mac](http://haskellformac.com) and code outside of [Haskell for Mac](http://haskellformac.com) playgrounds was developed using
[Atom configured for Haskell](http://achernyak.me/universal-haskell-dev-enviornment) and then laterly using [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=Vans.haskero).

See also [associated wiki](https://github.com/NickAger/LearningHaskell/wiki)

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
* [Practical Haskell and Purescript examples](https://lettier.github.io) eg [Making a movie monad)[https://lettier.github.io/posts/2016-08-15-making-movie-monad.html]
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
* [MonadIO Considered Harmful](https://chrispenner.ca/posts/monadio-considered-harmful) - defining class instances to specify exactly what IO operations are performed - interesting [discussion on redit](https://www.reddit.com/r/haskell/comments/703a55/monadio_considered_harmful/)
* [Rank 'n Classy Limited Effects](http://www.parsonsmatt.org/2016/07/14/rank_n_classy_limited_effects.html)- similar to the above but going further.
* [An opinionated guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/) - useful tips about running haddock and hoogle locally as well as a great summary of extensions. Also [redit discussion](https://www.reddit.com/r/haskell/comments/7wmhyi/an_opinionated_guide_to_haskell_in_2018/)
