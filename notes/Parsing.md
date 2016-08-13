
> 2016-05-03 16:52:50
>
> \<chipf0rk\>	I'm trying to parse everything before a comment
>
> \<chipf0rk\>	and that's the part that's tripping me up;
>
> \<nitrix\>	You can do manipulations like skipMany comments, or, optional comment, or sepBy notComment comment, etc.
>
> \<nitrix\>	Where notComment would be = manyTill anyChar (lookAhead (comment <|> eof))


### `manyTill`

```haskell
manyTill anyChar (string "--")
```
return all characters upto (but not including) "--" and moves the parse point after "--"
