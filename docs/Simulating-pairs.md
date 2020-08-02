Defining

```haskell
cons x y f = f x y
car xs = xs const
cdr xs = xs (flip const)
```

and the functions here behave like `car, cdr and Cons` in Scheme. 

In this manner, a bituple could be expressed as `bt = cons 2.5 "hello"`, and `car bt = 2.5`, `cdr bt = "hello"`. 

Also, a tuple with multiple elements (multi-tuples) coule be expressed as `mt = cons 1 (cons 2 (cons ..))`, and `car mt = 1, cdr mt = cons 2 (cons ..)`.

After we have got multi-tuples, lists could be expressed as `cons x (cons y ( .. (cons z nil) .. ))`, with defining `nil` to express an empty list. In this case, `car` and `cdr` could give the `head` and `tail` of the given list. List in Scheme is defined by this kind of recursive form.