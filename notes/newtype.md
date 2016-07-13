From Chapter 15, Monoid Semigroup:

In summary, why you might use newtype:

1. Signal intent: using newtype makes it clear that you only intend for it to be a wrapper for the underlying type. The newtype cannot eventually grow into a more complicated sum or product type, while a normal datatype can.
2. Improve type safety: avoid mixing up many values of the same representation, such as Text or Integer.
3. Adddifferenttypeclassinstancestoatypethatisotherwise unchanged representationally.


