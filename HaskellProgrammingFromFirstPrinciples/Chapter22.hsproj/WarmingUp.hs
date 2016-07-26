module WarmingUp where
  
import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev


fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  c <- cap
  r <- rev
  return (c, r)
  -- a <- _foo -- Found hole ‘_foo’ with type: [Char] -> t0
  -- _foo -- Found hole ‘_foo’ with type: [Char] -> ([Char], [Char])
  -- return (c,_foo) -- Found hole ‘_foo’ with type: [Char]

  
-- using typed holes we deduce that c and r are type [Char]
-- and "return (c,r)" is type [Char] -> ([Char], [Char])
-- conclusion: the monad is function application.
-- inside the monad we can 

{-
reminder: do syntax desugared:
do x <- mx
   y <- my
   z

-- ... is equivalent to:
do x <- mx
   do y <- my
      z

-- ... desugars to:
mx >>= (\x ->
my >>= (\y ->
z ))
-}

{-

ghc -ddump-simpl WarmingUp.hs 

tupledDo :: [Char] -> ([Char], [Char])
[GblId, Str=DmdType]
tupledDo =
  >>=
    @ ((->) [Char])
    $dMonad_rzt
    @ [Char]
    @ ([Char], [Char])
    cap
    (\ (c_awH :: [Char]) ->
       >>=
         @ ((->) [Char])
         $dMonad_rzt
         @ [Char]
         @ ([Char], [Char])
         rev
         (\ (r_awI :: [Char]) ->
            return
              @ ((->) [Char]) $dMonad_rzt @ ([Char], [Char]) (c_awH, r_awI)))
-}

tupledBind :: [Char] -> ([Char], [Char])
tupledBind = (>>=) 
              cap (\c -> (>>=)
                          rev (\r -> return (c , r)))
                          
tupledBind' :: [Char] -> ([Char], [Char])
tupledBind' = cap >>= (\c -> rev >>= (\r -> return (c , r)))
                          
{-
-- https://github.com/dmvianna/haskellbook/blob/master/src/Ch22-Reader.hs
tupled''' :: [Char] -> ([Char], [Char])
tupled''' xs = fmap rev $ (cap >>= (,)) (xs)
-} 

{-
-- https://lukleh.github.io/haskell-book-exercises/#_22_2_a_new_beginning
tupled_bind :: [Char] -> ([Char], [Char])
tupled_bind = rev >>= \x1 -> cap >>= \x2 -> return (x1, x2)

-}             


{-
nickager: “@let rev = reverse :: [Char] -> [Char]”
nickager: “@let tupledDo = do; c <- cap; r <- rev; return (c,r)”
nickager: > tupledDo "Monad confusion"
lambdabot:  ("MONAD CONFUSION","noisufnoc danoM")
nickager: I’m confused by the -> monad
PiDelport: nickager: You can think of Reader actions as projections of the environment value: every time you have a function from the environment type, it represents reading the environment, applying that function, and yielding the result.
nickager: is there a way I can find out what type “r” or “c” are in the above definitiion of “tupleDo"
PiDelport: nickager: They're the results of cap and rev, respectively.
PiDelport: nickager: Consider this: do x <- getLine, x <- getLine; return (x,y)
PiDelport: Do you understand IO actions like that?
nickager: my understanding is that inside the “do” you loose the “IO” so that x is “String” rather than “IO String”
nickager: (in your example)
nickager: still not sure about the “tupledDo” code
PiDelport: nickager: Pretty much, yeah.
PiDelport: nickager: Okay, so you intuitively understand how each getLine reads from standard input, and yields a line of text as x and y, respectively?
PiDelport: nickager: You can think of Reader actions in the same way, except that the Reader actions "read" the environment value.
nickager: more or less, I have a mental model - I’m not sure how accurate it is
PiDelport: > let foo = do x <- id; y <- id; return (x,y) in foo "bar"
lambdabot:  ("bar","bar")
PiDelport: In this case, id just reads the environment value unmodified
PiDelport: But you can use other functions too
PiDelport: > let foo = do x <- reverse; y <- map toUpper; return (x,y) in foo "bar"
lambdabot:  ("rab","BAR")
PiDelport: Another way to write the above functions, by the way, is with Applicative syntax: (,) <$> getLine <*> getLine
PiDelport: That's like (,) foo bar, except that "foo" and "bar" can be any Applicative action / value (IO in this case)
PiDelport: > let foo = (,) <$> id <*> id in foo "bar"
lambdabot:  ("bar","bar")
PiDelport: > let foo = (,) <$> reverse <*> map toUpper in foo "bar"
lambdabot:  ("rab","BAR")
PiDelport: That's equivalent to:
PiDelport: > let foo = \e -> (,) (reverse e) (map toUpper e) in foo "bar"
lambdabot:  ("rab","BAR")
PiDelport: where "e" is the environment value.
PiDelport: Does that help any?
nickager: I *think* I get the applicative examples, but where I’m struggling is my mental model tells me that inside a “do” the “x <- getLine” is stripping off the monad in that case “IO” but I don’t understand what the context is for “tupledDo = do; c <- cap; r <- rev; return (c,r)”. What type is “r”
monochrom: tupledDo x = let {c = cap x; r = rev x} in (c, r)
nickager: PiDelport: let foo = do x <- id; y <- id; return (x,y) in foo "bar" - I’m sure this should help - but I don’t get it at the moment
PiDelport: nickager: Think of the "id" actions as analogous to getLine
PiDelport: Except that instead of reading from standard input, they "read" the environment value that you pass to foo
PiDelport: In fact, another way to name it is:
PiDelport: > let foo = do x <- ask; y <- ask; return (x,y) in foo "bar"
lambdabot:  ("bar","bar")
PiDelport: :t ask
lambdabot: MonadReader r m => m r
PiDelport: Okay, that type isn't the most helpful, but basically, it's the same as id in that context: it just reads the environment value directly, without any additional changes.
nickager: “environment value” is different to parameter
nickager: ?
monochrom: same
PiDelport: nickager: "environment value" is just a name for it in the context of Reader actions.
PiDelport: To a Reader action, the environment value is the value that's input to the computation, and read whenever you use ask, id, or any other function to read it.
PiDelport: It's a global input that's accessible anywhere inside the Reader action, and its nested actions (without having to explicitly pass it along everywhere)
PiDelport: So if you look at for example: foo = \e -> (,) (reverse e) (map toUpper e)
nickager: I understand the use of the Reader, just not the interals
PiDelport: There, you're explicitly taking an environment value, and passing it along to each inner action (reverse and map toUpper)
PiDelport: Reader is just a way to avoid having to do that explicitly.
PiDelport: It makes the environment value implicit instead.
monochrom: nickager, do you want to see what >>= does?
nickager: but I just mechanically translated from do syntax to that, without really understanding it any more ,,,
PiDelport: nickager: In terms of implementation, it's pretty much exactly the translation I gave for foo above.
nickager: (where that is >>= syntax)
PiDelport: For Reader, (f <*> x) e = f e $ x e
PiDelport: In other words, f and x are Reader actions that expect an environment, and <*> applies them by taking an environment and passing it to both of them.
PiDelport: So result of (f e) ends up getting applied to the result of (x e)
nickager: OK
monochrom: OK. f >>= k  =  \x -> k (f x) x
nickager: I think I need to play around a little more before it makes sense
nickager: thanks both
monochrom: it may be less hairy to rewrite that as \x -> let {x1 = f x} in k x1 x
-}
