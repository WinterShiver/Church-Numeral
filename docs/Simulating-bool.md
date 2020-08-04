# 用函数（纯lambda表达式）表示Bool变量

非常简单，直接放代码。

```haskell
true = const -- true = \x y -> x
false = flip const -- false = \x y -> y
```

```haskell
not = flip
and b1 b2 = b1 b2 false
or b1 b2 = b1 true b2
xor b1 b2 = b1 (not b2) b2
```