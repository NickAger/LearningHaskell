### Parsers from first principles

see also:

* [NanoParsec](http://dev.stephendiehl.com/fun/002_parsers.html)
* [Monadic Parser Combinators](http://www.cs.nott.ac.uk/%7Epszgmh/monparsing.pdf)
* Scott Wlaschin (F# fame) - [Understanding Parser Combinators: A Deep Dive](https://skillsmatter.com/skillscasts/9731-understanding-parser-combinators-a-deep-dive)
* [Introduction to Parser Combinators (in F#)](http://santialbo.com/blog/2013/03/24/introduction-to-parser-combinators/)

From irc #haskell-beginners http://ircbrowse.net/browse/haskell-beginners?events_page=21897

Iceland_jack:

> A Parser for Things
> is a function from Strings
> to a List of Pairs
> of Things and Strings!


Cale:

> newtype Parser a = P (Text -> [(a, Text)])
> (I'm going to do it with Text rather than String this time just for fun, so import qualified Data.Text as T, and import Data.Text (Text))
> The idea here is that our parser is something which will try to consume some initial fragment of the Text, and produce a list of possible parses.
> Each parse will consist of a value of type a, and whatever is left of the string afterward
> If the list is empty, it means that the parser failed
> In many cases, the list will have just one element, but that need not be the case
> This representation makes it completely trivial to write something which will run our parser and get a list of the parses: it's really just a field extractor:
```
> runParser :: Parser a -> Text -> [(a,Text)]
> runParser (P f) s = f s
> or indeed
> runParser (P f) = f
> Or we could even just revise the newtype declaration:
> newtype Parser a = P { runParser :: Text -> [(a, Text)] }
> So, our goals will be to implement some basic parsers in this form, and some ways of gluing simpler parsers together into more interesting parsers
> until we perhaps no longer need to use this P data constructor directly
> (but at the start, it's going to be our only way to build a parser)
> So, I think the first primitive parser I want to write is something I call separate
> separate :: (Text -> Maybe (a, Text)) -> Parser a
> There are a bunch of functions in Data.Text which fit the mold of (Text -> Maybe (a, Text)) for partitioning a Text value into some initial part, and a remainder
> Or things which can be used to get that anyway
> So
> separate f = ...
> We know we're writing a Parser a, so we might as well use the data constructor, given that we don't have many other ways to make Parser values yet
> separate f = P ...
> and then it takes a function Text -> [(a,Text)], so that might as well be a lambda
> separate f = P (\s -> ...)
> So now we have our input string s, and what we're going to do is apply f to it, and case on the result
> separate f = P (\s -> case f s of Nothing -> ...; Just (v,s') -> ...)
> If f s produces Nothing, our parser fails, which we'll represent by an empty list
> Just (a, t) -> [(a, t)]; Nothing -> []
> So, now how about string :: Text -> Parser Text
> We have from Data.Text, the function T.stripPrefix :: Text -> Text -> Maybe Text
> T.stripPrefix s t will try to see if s occurs at the beginning of t, and if so, it will give the remainder of t, following that
> (it'll give Just of that)
> and if not, it gives Nothing
> So, let's consider fmap ((,) s) (T.stripPrefix s t)
> Or (\t -> fmap ((,) s) (T.stripPrefix s t))
> this is a function Text -> Maybe (Text, Text)
> if we were to apply it to, say, "foo" and "foobar", it would give us Just ("foo", "bar")
> and if we were to apply it to "foo" and "quux", we'd get Nothing
> So, this is the kind of thing that separate makes a parser from
> string s = separate (\t -> fmap ((,) s) (T.stripPrefix s t)) :: Parser Text
> So, string s will be a parser which tries to find s at the beginning of the input
> and if it succeeds, then it does so uniquely, producing s as its result, and the remainder of the input as you'd expect
> and if it doesn't find s, then stripPrefix results in Nothing, and separate will make that into an empty list of parses
> We can also do spanP :: (Char -> Bool) -> Parser Text
> spanP p = separate (Just . T.span p)
> T.span :: (Char -> Bool) -> Text -> (Text, Text)
> will split a Text into an initial portion all of whose characters satisfy the given predicate
> and whatever remains afterward
> We call our helper `separate` because it turns something which splits things into a parser
> If we wanted our parsers to, say, keep track of the character position in the input, then it would have to do something a bit more
> We're going to make this Parser type into a monad
> and that's going to give us a way to combine parsers together into more sophisticated ones
> So, we could implement some more primitives, but let's just get right to it
> return :: a -> Parser a
> will be a parser which always succeeds uniquely, without having consumed any input
> and will produce the given value as its unique result
> We could actually use separate to implement this, but let's just use the data constructor P
> return v = P (\s -> ...)
> So remember P takes a function Text -> [(a, Text)]
> and we said we want the thing so succeed uniquely, so this is going to be a one-element list we're producing
> return v = P (\s -> [...])
> we want the remainder of the string to be the entire input string
> and the result is v
> return v = P (\s -> [(v,s)])
> Now I'm going to foreshadow something here by pointing out that producing a one-element list is what return for the list monad does
> So we could also write
> return v = P (\s -> return (v,s))
> where the return on the right hand side is the return for the list monad
> Okay, so let's tackle (>>=)
> I'll write the type down, and then explain what we want it to produce
> (>>=) :: Parser a -> (a -> Parser b) -> Parser b
> So, when we have
> p :: Parser a
> and
> f :: a -> Parser b
> we want
> p >>= f :: Parser b
> To be the parser which first parses the initial portion of the input using p
> and then for each successful parse (v,s') consisting of a value v :: a, and depleted input string s' :: Text
> it will run the parser f v on the string s'
> and it will collect up the results, doing this in all possible ways
> each of the parses (w,s'') from running f v on s' will be one of its parses
> So, this needing to do things in all possible ways is a sign that maybe the list monad would be helpful
> If you've seen the plain State s monad before, then it might be slightly familiar
> i.e. it follows the same pattern of "run the first thing, and apply the function to its result to get the second thing to run"
> So:
> p >>= f = P(\s -> ...) -- we're start off the same way as before
> now, we want to first run p on the initial input s, and do something for each of the resulting parses, so let's use do-notation for the list monad:
> p >>= f = P (\s -> do (v,s') <- runParser p s; ...)
> So we get some result v, and depleted input s'
> and then we want to run the parser (f v) on the depleted input s' ...
> p >>= f = P (\s -> do (v,s') <- runParser p s; (w,s'') <- runParser (f v) s'; ...)
> and its results will be our own:
> p >>= f = P (\s -> do (v,s') <- runParser p s; (w,s'') <- runParser (f v) s'; return (w,s''))
> we can save the binding to (w, s'') and returning
> p >>= f = P (\s -> do (v,s') <- runParser p s; runParser (f v) s')
> So, remember we also had
> return v = P (\s -> return (v,s))
> Note that nothing here, apart from the type of P itself says that we're using lists
> So, this hints at the fact that maybe we could be doing something more general instead
> Well, given that we know that runParser p s is a list
> you should read (v,s') <- runParser p s as being like "let (v,s') be selected from the list runParser p s (in all possible ways)"
> and we're letting the list monad instance take care of iterating through all the possible choices of selections (using concat and map)
> It's the exact same thing as writing a list comprehension:
> [(w,s'') | (v,s') <- runParser p s, (w,s'') <- runParser (f v) s']
> Though there are indeed, in general going to be zero or more ways to make the selection of the result from runParser p s, and the result from runParser (f v) s'
> and what options we have from the second one will depend on the choice we made from the first
> For example, we're eventually going to have things like many p which parses zero or more copies of the parser p
> and so if the first parser is many p, it might provide very different depleted strings to what follows, depending on how many copies of p we take
> (and also, f v will be different in each case, which might make a difference to how we decide to parse what remains)
> There will also implicitly be a convention that we're going to prefer the results at the beginning of the lists
> usually you will only care about the first successful parse
> This is indeed very similar to the definition of the state monad structurally, Compare:
> newtype State s a = S { runState :: s -> (a,s) }
> newtype Parser a = P { runParser :: Text -> [(a, Text)] }
> For State s, we have
> return v = S (\s -> (v,s))
> For Parser we have
> return v = P (\s -> return (v,s))
> For State s, we have
> x >>= f = S (\s -> let (v,s') = runState x s; (w, s'') = runState (f v) s' in (w, s''))
> and for Parser, we have
> x >>= f = P (\s -> do (v,s') <- runParser x s; (w, s'') <- runParser (f v) s'; return (w, s''))
> So the only differences between these two is that
> 1) We've introduced this monad, the list monad, and we're not actually using the fact that it is definitely the list monad, we're just using its return and (>>=)
> 2) We've specialised s to Text
> So, let's follow our nose here, and generalise Parser over both the choice of Text that we made somewhat arbitrarily, and more interestingly, the choice of monad to use in place of lists
> newtype StateT s m a = StT { runStateT :: s -> m (a,s) }
> (note, it's common to name the data constructor the same as the type here, but I'm abbreviating it so that it's always clear what I'm talking about for the purposes of the discussion)
> So, now we just steal the definitions of return and (>>=) we already wrote for Parser, and just note that they typecheck
> return v = StT (\s -> return (v,s))
> x >>= f = StT (\s -> do (v,s') <- runStateT x s; (w, s'') <- runStateT (f v) s'; return (w, s''))
> or, of course, we can simplify using the right unit law of a monad:
> x >>= f = StT (\s -> do (v,s') <- runStateT x s; runStateT (f v) s')
> So that gives us that Parser is the same thing as StateT Text []
> Well, the "same thing", but with a bunch of parsing-specific primitives built up in a library.
> and a sort of convention that we're typically only ever going to change the state such that it becomes a suffix of what we had before
> So, with that aside, let's at least briefly go back to the parser implementation and add some things
> We can get Functor by setting fmap = liftM, and Applicative by setting pure = return and (<*>) = ap
> But we'll also typically want Alternative as well
> with empty being the parser which always fails
> and p <|> q being the parser which combines the results of parsing with p and q
> starting from the same point
> ie empty = P (\s -> [])
> p <|> q = P (\s -> runParser p s ++ runParser q s)
> Another really good primitive often to start with for parsers is satisfy
> satisfy :: (Char -> Bool) -> Parser Char
> tries to match a single character
> and it has to be a character which satisfies the given predicate
> satisfy p = P (\s -> case T.uncons s of Just (x,s') | p x -> [(x,s')]; _ -> [])
> (This is a slightly more natural operation on String than on Text)
> char c = satisfy (== c)
> char :: Char -> Parser Char
> digit :: Parser Char
> digit = satisfy isDigit
> letter = satisfy isAlpha
> With a String rather than Text implementation of Parser, we might have
> string = mapM char
> But that's not the most efficient thing to do with a Text
> So we could have written
> newtype Parser a = P (StateT Text [] a) deriving (Functor, Applicative, Alternative, Monad, MonadPlus)
> and then write runParser (P x) s = runStateT x s
> and then perhaps formulate stuff like separate a little bit differently
> separate f = P (do s <- get; case f s of Nothing -> empty; Just (v,s') -> put s' >> return v)
> and of course, we can have stuff like
> eof :: Parser ()
> eof = P (do s <- get; guard (T.null s))
> using the new StateT definition
> With the non-StateT version, we'd have eof = P (\s -> if T.null s then [()] else [])
> Indeed, that's the same as guard (T.null s) for the list monad
> So eof = P (\s -> guard (T.null s)) is yet another option for that style
>
```
