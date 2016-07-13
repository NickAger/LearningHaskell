From Chapter 15, "Monoid, Semigroup"


> Orphan instances are still a problem even if duplicate instances aren’t both imported into a module because it means your typeclass methods will start behaving differently depending on what modules are imported, which breaks the fundamental assumptions and niceties of typeclasses.


There are a few solutions for addressing orphan instances:

1. You defined the type but not the typeclass? Put the instance in the same module as the type so that the type cannot be imported without its instances.
2. You defined the typeclass but not the type? Put the instance in the same module as the typeclass definition so that the typeclass cannot be imported without its instances.
3. Neither the type nor the typeclass are yours? Define your own newtype wrapping the original type and now you’ve got a type that “belongs” to you for which you can rightly define typeclass instances. There are means of making this less annoying which we’ll discuss later.
