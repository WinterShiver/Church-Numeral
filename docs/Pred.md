# 在Church Numeral中表示自然数前驱（decr，pred）

`decr`，或称`pred`，即自然数的前驱，比如6的前驱是5. 在自然数数据类型`Church`中，由于前驱运算`decr`不是封闭的，因此实现起来有点复杂。

本文介绍通过“从-1向前数”这种思路实现的`decr`，并用Haskell语言实现。

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

以上是本文代码实现中使用的`Church`数据类型。本文对应的代码实现是[Pred.hs](../code/Pred.hs)，为了Haskell代码能够通过type check，在代码实现中使用这种基于包裹类型的`Church`类型，相应地牺牲了一定的纯粹性。[Pred.scm](../code/Pred.scm)使用Scheme语言，对`decr1c`这个可以用纯lambda实现的函数进行了实现。

# 从-1开始数：用特殊值表示-1

可以通过`Maybe`或者`(,)`增广`Church`类型，为`Church`设置一个`-1`，并在增广的类型上设置一个新的`incr`用来计算后继，从`-1`计算后继`n`次就是`decr n`. 在这里，新的`incr`用函数名尾部的数字和原来的`incr`进行区别。

## 使用Maybe类型表示-1

使用`Maybe`的时候，可以以`Nothing :: Maybe Church`为`-1`. 为增广过的`Maybe Church`类型实现一遍`incr`，然后通过对`-1`应用n次`incr`获得前驱。

```haskell
type MaybeCh = Maybe Church

incr1 :: MaybeCh -> MaybeCh
incr1 Nothing = Just zero
incr1 (Just ch) = Just (incr ch)
-- incr1 Nothing = Just zero, incr1 (Just zero) = Just one, ..

decr1 :: Church -> MaybeCh
decr1 (Ch n) = n incr1 Nothing 
-- decr1 zero = Nothing, decr1 one = Just zero, decr1 two = Just one, ..
```

效果如下

```bash
*Pred> ch2num <$> decr1 zero
Nothing
*Pred> ch2num <$> decr1 one
Just 0
*Pred> ch2num <$> decr1 two
Just 1
*Pred> ch2num <$> decr1 three
Just 2
```

## 使用二元组类型表示-1

使用`(,)`是使用一个二元组容纳一个数的前驱和它本身，在每次函数调用的时候用这个数的后继和这个数本身的值替换原值，用类似滚动数组的方式更新二元组中容纳的数值。在这里起到`-1`作用的是`(undefined, zero)`，使用`incr2`获得它的后继`(zero, one)`, `(one, two)`等等，做n次得到`(n-1, n)`，取`fst`获得Church数的前驱。

```haskell
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
*Pred> ch2num (decr2 zero)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
  undefined, called at Pred.hs:32:26 in main:Pred
*Pred> ch2num (decr2 one)
0
*Pred> ch2num (decr2 two)
1
*Pred> ch2num (decr2 three)
2
```

# 从-1开始数：用lambda表达式表示

因为`decr`在自然数集合上不是封闭的，0的前驱是负数，所以刚才的两种实现`decr1`和`decr2`都对`decr 0`进行了异常处理。借助和类型引入`-1`，增广了自然数集合的范围，然后`decr`从原来的类型`Church`映射到增广的类型，把`0`的前驱映射到`-1`，从而实现定义。
* 在`decr1`中，`Church`增广成`Maybe Church`，使用`Nothing`表示`decr 0`，使用`Just ch`表示正常的数。
* 在`decr2`中，`Church`增广成`(Church, Church)`，使用`(undefined, zero)`表示`decr 0`，正常的数分别对应自己的前驱及自己本身组成的元组。

实际上，还有另外一种处理方法。我们可以在不增广自然数范围的基础上，对`decr`在`zero`处的取值进行增补，为`decr 0`赋予一个合理的自然数值，使`decr`变为在自然数集上封闭的函数。我们在名字加一撇区别于原来，`decr' :: Church -> Church`是在`zero`处也有返回值的一个total function，这样的`decr'`是可以用纯lambda表示的，而不需要类型的帮助。

## 实现

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
newtype CPSCh = CPSCh {
  runCPSCh :: forall r. (Church -> r) -> r -> r
}

cpsNothing :: (a -> r) -> r -> r
cpsNothing ks kf = kf

cpsJust :: a -> (a -> r) -> r -> r
cpsJust a ks kf = ks a
 
incr1c :: CPSCh -> CPSCh
incr1c (CPSCh c) = CPSCh (cpsJust (c incr zero))
-- incr1c cpsNothing = cpsJust zero, incr1c (cpsJust zero) = cpsJust one, ..

decr1c' :: Church -> CPSCh
decr1c' (Ch n) = n incr1c (CPSCh cpsNothing)
-- decr1c' zero = cpsNothing, decr1c' one = cpsJust zero, ..

decr1c :: Church -> MaybeCh
decr1c ch = c Just Nothing
  where (CPSCh c) = decr1c' ch
-- decr1c zero = Nothing, decr1c one = Just zero, ..

decr'1c :: Church -> Church
decr'1c ch = c id zero
  where (CPSCh c) = decr1c' ch
-- decr'1c zero = zero, decr'1c one = zero, decr'1c two = one, ..
```

效果如下

```bash
*Pred> ch2num <$> decr1c zero
Nothing
*Pred> ch2num <$> decr1c one
Just 0
*Pred> ch2num <$> decr1c two
Just 1
*Pred> ch2num <$> decr1c three
Just 2
```

```haskell
*Pred> ch2num (decr'1c zero)
0
*Pred> ch2num (decr'1c one)
0
*Pred> ch2num (decr'1c two)
1
*Pred> ch2num (decr'1c three)
2
```

# 更多资料

本文主要参考[如何理解丘奇计数的减法？ - HOOCCOOH的回答 - 知乎](https://www.zhihu.com/question/64274105/answer/218817959)

另有实现`decr`的其他方法，列举在这里：
* [如何理解丘奇计数的减法？ - UWRF的回答 - 知乎](https://www.zhihu.com/question/64274105/answer/221969009)
* [Church Numerals 中的 PRED 是怎么来的](https://zhuanlan.zhihu.com/p/93343864)
* [Church encoding - Derivation of predecessor function](https://en.wikipedia.org/wiki/Church_encoding#Derivation_of_predecessor_function)