# 用函数（纯lambda表达式）表示列表和元组

```haskell
cons x y f = f x y
car xs = xs const
cdr xs = xs (flip const)
```

这几个pure lambda的函数可以模仿Scheme中的`car`, `cdr`和`Cons`.

在这样的函数定义下，可以用形如`b = cons 2.5 "hello"`方式表示二元组，不难见`car b = 2.5`, `cdr b = "hello"`. 

多元组可以用二元组嵌套的方式表示，如`m = cons 1 (cons 2 (cons ..))`, `car m = 1, cdr m = cons 2 (cons ..)`.

基于多元组，还可以表示列表。我们需要引入一个空列表符号`nil`，则列表`[1, 2 .. 10]`可以表示成`xs = cons 1 (cons 2 ( .. (cons 10 nil) .. ))`的形式。此时`car xs = 1, cdr xs = cons 2 ( .. (cons 10 nil) ..)`，和Haskell中的`head`和`tail`表现一致。