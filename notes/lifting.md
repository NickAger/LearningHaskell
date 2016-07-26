From Chapter 22:

> With the Either Functor we known that we will lift over the `Either a` and if our function will be applied, it will be applied to the `b` value.
> With the function type:
>
> `data (->) a b`
>
> the same rule applies; you have to lift over the `(->) a` and only transform the `b` value.
