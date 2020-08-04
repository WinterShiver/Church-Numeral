# 在Church Numeral中表示自然数前驱（decr，pred）

`decr`，或称`pred`，即自然数的前驱，比如6的前驱是5. 本文针对自然数数据类型`Church`讨论分析几种前驱（`decr`）的实现方法。

```haskell
-- Definition of Church
newtype Church = Ch {
  runCh :: forall a. (a -> a) -> a -> a
}

zero :: Church
zero = Ch (\s z -> z)  

incr :: Church -> Church
incr (Ch n) = Ch (\s z -> s (n s z)) 
```

为了提供可以正常执行的Haskell代码，在代码实现中使用这种基于包裹类型的`Church`类型。这可能导致代码中相应地牺牲了一定的纯粹性；后续工作中会基于Scheme给出“更纯粹”的实现。

# 两种实现思路

因为`decr`在自然数集合上不是封闭的，0的前驱是负数，所以对`decr`比较合理的设计主要分为两种：

第一种，对`decr 0`进行异常处理。例如，可以定义`decr :: Church -> Maybe Church`. 对`zero`来说，求`decr`是负数，不在自然数范围内，所以返回`Nothing`作为`-1`；对其他数来说，可以正常返回自己之前的数（以`Just`包裹）。其他类似的操作也一并归于此类。


第二种，对`decr`在`zero`处的取值进行增补，形成`decr'`，名字加一撇区别于原来。`decr' :: Church -> Church`在`zero`获得一个合理的返回值作为增补，从而变成一个total function. 

以下用`decr`和`decr'`的函数名区别两种策略。

# 从-1开始数 & 用特殊值表示-1

可以通过`Maybe`或者`(,)`增广`Church`类型，为`Church`设置一个`-1`，并在增广的类型上设置一个新的`incr`用来计算后继，从`-1`计算后继`n`次就是`decr n`. 在这里，新的`incr`用函数名尾部的数字和原来的`incr`进行区别。

## 使用Maybe类型表示-1

使用`Maybe`的时候，可以以`Nothing :: Maybe Church`为`-1`. 为增广过的`Maybe Church`类型实现一遍`incr`，然后通过对`-1`应用n次`incr`获得前驱。

```haskell
-- Method 1
-- Count from -1: using Maybe Church

type MaybeCh = Maybe Church

incr1 :: MaybeCh -> MaybeCh
incr1 Nothing = Just zero
incr1 (Just ch) = Just (incr ch)

decr1 :: Church -> MaybeCh
decr1 (Ch n) = n incr1 Nothing 

-- decr1 zero = Nothing, decr1 one = Just zero, decr1 two = Just one, ..
```

效果如下

```bash
*Pred> ((\n -> n (*2) 1) . runCh <$> (decr1 zero)) 
Nothing
*Pred> ((\n -> n (*2) 1) . runCh <$> (decr1 one)) 
Just 1
*Pred> ((\n -> n (*2) 1) . runCh <$> (decr1 two)) 
Just 2
*Pred> ((\n -> n (*2) 1) . runCh <$> (decr1 three)) 
Just 4
```

## 使用二元组类型表示-1

使用`(,)`是使用一个二元组容纳一个数和它的前驱，在每次函数调用的时候用这个数的后继和这个数本身的值替换原值，用类似滚动数组的方式更新二元组中容纳的数值。在这里起到`-1`作用的是`undefined`.

```haskell
-- Method 2
-- Count from -1: using Tuple Church

type TupleCh = (Church, Church)

incr2 :: TupleCh -> TupleCh
incr2 (_, ch) = (ch, incr ch)

decr2' :: Church -> TupleCh
decr2' (Ch n) = n incr2 (undefined, zero) 
-- decr2' zero = (u, zero), decr2' one = (one, two), ..

decr2 :: Church -> Church
decr2 = fst . decr2'
-- decr2 zero = u, decr2 one = zero, decr2 two = one, ..
```

效果如下

```bash
*Pred> runCh (decr2 zero) (*2) 1
*** Exception: Prelude.undefined (关于undefined错误的一些描述)
*Pred> runCh (decr2 one) (*2) 1
1
*Pred> runCh (decr2 two) (*2) 1
2
*Pred> runCh (decr2 three) (*2) 1
4
```

# 从-1开始数：用lambda表达式表示

```haskell
type MaybeCh = Maybe Church

incr1 :: MaybeCh -> MaybeCh
incr1 Nothing = Just zero
incr1 (Just ch) = Just (incr ch)

decr1 :: Church -> MaybeCh
decr1 (Ch n) = n incr1 Nothing 
```

基于上面这段代码继续讨论。如果使用untyped lambda calculus来定义，`Maybe Church`类型是不能使用的，我们需要一种另外的方式表示这种结构。

这个时候我们想到了CPS变换，可以用来表示和类型。关于如何用CPS表示和类型，我做过一个相关的工作[repo](https://github.com/WinterShiver/CPS-Type)，但是里面说的不是特别清楚（其实我现在也没搞懂CPS的背景、提出的思路和正式的形式）。不过无论如何，我们可以用如下的方式把`Nothing`和`Just ch`表示成对应的lambda表达式：

```haskell
cps Nothing = \ks kf -> kf
cps (Just ch) = \ks kf -> ks ch
```

注意到此时的`cps :: MaybeCh -> ((Church -> r) -> r -> r)`. 但是这里说`cps`的类型只是让读者知道形如`\ks kf -> xx`的结构，在下文中我们不会刻意注意类型。

那么下面我们要为其定义对应的后继和前驱函数。首先考虑前驱函数`incr1c`，需要做到

```haskell
incr1c (\ks kf -> kf) = \ks kf -> ks zero
incr1c (\ks kf -> ks ch) = \ks kf -> ks (incr ch)
```

很容易推得

```haskell
incr1c c = \ks kf -> ks (c incr zero)
```

然后对应地把原来的`decr1`对应地转化为CPS后的`decr1c`. 先经历一个中间步骤`decr1c'`:

```haskell
decr1c' ch = ch incr1c (\ks kf -> kf)
```

`decr1c'` 接收一个church数，返回一个CPS变换后的Church数。在`ch = zero`时，定义式右侧对CPS Church的`-1`（即`\ks kf -> kf`）应用0次`incr1c`，得到CPS Church的`-1`本身；在`ch = n`时，定义式右侧对CPS Church的`-1`应用n次`incr1c`，得到CPS Church中n的前驱。

然后就可以根据`decr1c'`的返回值计算`decr1c`. 

如果要求`decr1c`满足`decr1c zero = Nothing`, `decr1c (incr ch) = Just ch`，则

```haskell
decr1c ch = decr1c' ch Just Nothing
```

如果要求`decr1c`满足`decr1c zero = zero`, `decr1c (incr ch) = ch`，则

```haskell
decr'1c ch = decr1c' ch id zero
```

综上所述，我们可以用以下方式实现纯lambda定义`pred`：

```haskell
incr1c c = \ks kf -> ks (c incr zero) 
decr1c' ch = ch incr1c (\ks kf -> kf)
decr1c ch = decr1c' ch Just Nothing 
-- decr1c zero = Nothing, decr1c one = Just zero, ..
decr'1c ch = decr1c' ch id zero
-- decr'1c zero = zero, decr'1c one = zero, decr'1c two = one, ..
```

`decr'1c`可以作为纯lambda表达式在scheme中实现，代码可见于`Pred.scm`，此处不再粘贴，自己去看原文件。效果如下：

```bash
> (load "Pred.scm")
> (toInt (decr1c zero))
0
> (toInt (decr1c one))
0
> (toInt (decr1c two))
1
> (toInt (decr1c three))
2
```

在haskell中，为了和类型系统做妥协，我们还是需要使用包裹类型，代码最终是这个样子：

```haskell
-- Method 1c
-- Count from -1: pure lambda version
-- This form is deducted by CPS transform. See `Pred.md`.

newtype CPSCh = CPSCh {
  runCPSCh :: forall r. (Church -> r) -> r -> r
}

incr1c :: CPSCh -> CPSCh
incr1c (CPSCh c) = CPSCh (
  \ks kf -> ks (c incr zero) 
  )

decr1c' :: Church -> CPSCh
decr1c' (Ch n) = n incr1c (CPSCh (\ks kf -> kf))

decr1c :: Church -> MaybeCh
decr1c ch = let c = runCPSCh (decr1c' ch) in c Just Nothing

decr'1c :: Church -> Church
decr'1c ch = let c = runCPSCh (decr1c' ch) in c id zero
```

效果如下

```bash
*Pred> (\n -> n (*2) 1) <$> (runCh <$> decr1c zero)
Nothing
*Pred> (\n -> n (*2) 1) <$> (runCh <$> decr1c one)
Just 1
*Pred> (\n -> n (*2) 1) <$> (runCh <$> decr1c two)
Just 2
*Pred> (\n -> n (*2) 1) <$> (runCh <$> decr1c three)
Just 4
```

```haskell
*Pred> runCh (decr'1c zero) (*2) 1
1
*Pred> runCh (decr'1c one) (*2) 1
1
*Pred> runCh (decr'1c two) (*2) 1
2
*Pred> runCh (decr'1c three) (*2) 1
4
```
