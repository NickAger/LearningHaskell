From Chapter 15, Monoid Semigroup:

In summary, why you might use newtype:

1. Signal intent: using newtype makes it clear that you only intend for it to be a wrapper for the underlying type. The newtype cannot eventually grow into a more complicated sum or product type, while a normal datatype can.
2. Improve type safety: avoid mixing up many values of the same representation, such as Text or Integer.
3. Add different type class instances to a type that is otherwise unchanged representationally.


> For veteran programmers that understand pointers newtype is like a single-member C union that avoids creating an extra pointer, but still gives you a new type constructor and data constructor so you don’t mix up the many many many things
that share a single representation.

See also "Phantom types" in [Four months with Haskell](http://lexi-lambda.github.io/blog/2016/06/12/four-months-with-haskell/)
