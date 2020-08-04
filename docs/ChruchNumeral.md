# Understanding Church numeral in 20 minutes

This post makes you understand the details of Church Numeral, including the declaration and illustration of its design and relevant proof in the definition. This post only contains the crucial information for constructing the system of Church Numeral, it could be seen as a complection and a summary for [^1]. 

All lambda expressions are written in Haskell style, but they may not be runable Haskell code. See implementation in  [Church.hs](../code/Church.hs) and [ChurchAdvanced.hs](../code/ChurchAdvanced.hs).

# Church numeral: functionally defining natural numbers

In Church numeral, natural number `n` is defined as a function which receives function `s` and initial value `z` as arguments, and returns the result of applying `s` to `z` n times. Eg. `zero s z = z, one s z = s z, two s z = s (s z), ..`.

By Peano's axioms, the scheme that implements natural numbers could be defined by defining `zero` and `incr`. Here goes the definition.

```haskell
zero = \s z -> z
```

Understanding `zero`: apply `s` to `z` zero times. 

```haskell
incr = \n s z -> n s (s z)
-- or: incr = \n s z -> s (n s z)
```

Understanding `incr`: `incr n s z = n s (s z)` implies that `incr n s z` applying `s` to `s z` for n times, which is equivalent in effect to applying `s` to `z` n+1 times. Similarly, in evaluating `s (n s z)`, `s` is applied one more time after being applied to `z` for n times, totally n+1 times is `s` applied, so `incr n s z = s (n s z)` also makes sense.

```haskell
incr n s = s . n s -- or n s . s
```

The equivalent definition above hints an another way to understand `incr`: notice that `n s` as a whole, is the function who receives the initial value `z`, and returns the result applying `s` to `z` n times. In this case, the application of `n` to `s` does the composition of n `s`s, i.e. `n s = s . s . .. . s`, in the right of which there are n `s`s included in the composition. We note `n s = s . s . .. s` as `n-composed-s` in below.

In this point of view, applying `incr n` to `s` makes `n+1-composed-s`, noting recursively as `s . n s` as the composition of `s` and `n-composed-s`, or `n s . s` as the composition of `n-composed-s` and `s`. Here `s . n s = n s . s` since `(.)` is associative. 

The original definition of `zero` and `incr` becomes `zero = const id` and `incr = liftA2 (.) id` after a point-free style modification. Here `liftA2` is defined on `Applicative ((->) r)`, in which `liftA2 f g h x = f (g x) (h x)`.

# Addition and multiplication

## Addition (add)

```haskell
add n m s z = n s (m s z)
```

Understanding `add`: applying `s` m times to `z` attains `m s z`, and applying `s` n times to `m s z` attains `n s (m s z)`. In total `s` is applied n+m times to `z` in `add n m s z`, proving the correctness of the definition.

From another point of view, `add n m s z = n s . m s $ z`. `n s . m s = (s . s .. s) . (s . s .. s)`, in which n `s`s in the front and m `s`s in the back. Noticing that the composing function `(.)` is associative, so the bracket is not needed, `n s . m s` equivalents to the composition of n+m `s`s, which is `add n m s`. The definition still makes sense.

The original definition `add n m s z = n s (m s z)` becomes `add = liftA2 (.)` after a point-free style modification, where `liftA2 f g h x = f (g x) (h x)`.

## Multiplication (mul)

```haskell
mul n m s z = n (m s) z
```

Understanding `mul`: the function `m s` applies `s` m times to `z`, and the function `n` applies `m s` n times to `z`, so `n (m s) z` returns the result that applies `s` m\*n times to `z`. 

If you still could not understand this, you could treat `m s` as a "super successor" (as said in [^1]), and `n` process this super-succession for n times on `z`, resulting the same effect with processing `s` for n\*m times on `z`. 

The original definition of `mul` becomes `mul = (.)` or `mul n m = n . m` after point-free-ize. 

# Power (pow)

Before going on, you should make sure that you understand the usage of `n` that `n` takes a function `s` as the argument, and returns the composition of n `s`s (`n-composed-s`), as `n = \s -> s . s .. s` (in which there are n `s`s). This explains why `n (m s)` means the composition of n\*m `s`s. 

And here comes the definition of `pow`: 

```haskell
pow n m s = m n s
```

Understanding `pow`: `m n s` means applying `n` m times to `s`. In m times application, the first application of `n` to `s` returns the composition of n `s`s, which is noted as `n-composed-s`. After that, the second application of `n` is the composition of n `n-composed-s`s, results in the composition of n\*n `s`s. It could be inducted that m times application of `n` to `s` returns the composition of `n*n*..*n` `s`s, which is the composition of `n^m` `s`s. 

As a supplementary, if m is zero, then `pow n zero s = zero n s = s`, which implies `pow n zero = one`.

If you are still confused on the form of `pow`, things might be easier with the latter form.

```haskell
pow n m = m (mul n) one
```

This form is easily being understood. Based on this form, a proof of `pow n m s z = m n s z` could be bulit by a mathmatical induction on `m`. The first step is to prove `pow n zero s z = one s z`, and the second step is to prove `pow n (incr m) = (incr m) n` when `pow n m = m n`.

All these above are completely enough to prove the correctness of `pow`'s definition. By the end we should mention that the original definition becomes `pow = (&)` after a point-free-ize operation.

# Predecessor and subtraction

## Predecessor (decr)

```haskell
decr n s z = n (\g h -> h (g s)) (const z) id
```

Understanding `decr`: Current I have found [^2] and [^3] being inspirational.

Note that `decr zero` happens to be `zero`. This is crucial for the implementation of `sub`.

## Subtraction (sub)

```haskell
sub n m = m decr n
```

`sub n m` is decreasing `n` for m times. If `n<m`, the result value would become `zero` in one intermediate step, and the evaluation would return `zero` eventually.

Note that `decr` and `sub` in the traditional sense are not closed on natural numbers (for example `decr 0 = -1` and `-1` is not a natural number), but by our implementation, in the situations `pred n` or `sub n m` returning negative numbers, they return `zero` instead. This makes the modified `decr` and `sub` to be total functions.

# Comparison

Defining comparison by judging the value of `sub n m` and `sub m n`. 

* If `sub n m /= zero`, then `n > m`;
* If `sub m n /= zero`, then `n < m`;
* Otherwise `n == m`.

# The type problem

In [Church.hs](../code/Church.hs) we are not able to construct a valid type for `sub` (see the type signature of `add`, `mul`, `pow`, `decr` and consider why). As said in [^1], we should construct a wrapped type `newtype Church = Ch (forall a. (a -> a) -> a -> a)`. The new implementation is in [ChurchAdvanced.hs](../code/ChurchAdvanced.hs).

# References

[^1]: [Church numerals: a tutorial](https://karczmarczuk.users.greyc.fr/Essays/church.html)
[^2]: [如何理解丘奇计数的减法？ - UWRF的回答 - 知乎](https://www.zhihu.com/question/64274105/answer/221969009)
[^3]: [Church Numerals 中的 PRED 是怎么来的](https://zhuanlan.zhihu.com/p/93343864)