---
title: Monad 的意义
published: 2012-07-07
excerpt: 
tags: Haskell, Monad
---

## Haskell 的意义

学习 haskell, 基本上大家的第一个反应是学这东西有什么用? 也是, 学习一个东西之前知道这玩意有啥用, 才有动力去学. 想当年, 刚开始学线性代数的时侯, 矩阵变换来变换去, 我就一直耿耿于怀: 变来变去搞什么名堂, 有啥子用? 因此, 线性代数就没学好. 考试考完就全忘了. 后来才发现这玩意用处还不小: 可以解多元方程, 可意做图形学上的空间变换, 重新捡起书本, 我才对线性代数有了点印象. 学习 Haskell 有甚么用处呢. 现在想来, 用处也不大, 主要还是兴趣. Haskell 是一门很优美的语言, 虽然也有一些来源于兼容性的妥协, 但是大体上而言, Haskell 简单, 一致, 正交, 静态类型, 高性能, 基本符合个人对一门完美编程语言的要求. Monad 的意义

任何学习 Haskell 的一大成就或者说一大障碍就是 Monad. 好多人学 Haskell 学到 Monad 这一章不禁疑窦丛生: 这劳什子做什么的, 未啥别的语言里面都没遇到过? 下面我就试试来说说 Monad 有甚么意义.

## 抽象是程序设计的根本方法

CPU 是电路的的抽象, 汇编是 CPU (硬件) 抽象, 操作系统(系统调用)是汇编的抽象. C 语言是操作系统的抽象.

当程序设计发展到今天, 一个人不太可能象 Donald E. Knuth 一样通晓所有算法. 不太可能象 Bill Joy 一样 3 天写出个操作系统. 各种繁复的功能和硬件被抽象成了几个类库让大家去调用. 大家只要知道类库怎么使用就行了, 不需要知道其实现的原理.

## Haskell 的抽象方法 : Functor

Haskell 可以使用 ADT 做数据抽象, 例如:

    data Maybe a = ...
    data [a] = ...
    data IO a = ...

代表的分别是 可能不存在的 a, a 类型的一个列表, a 类型的一个 IO. 具体 `Maybe`, `[]`, `IO` 是怎么定义的实际上我们都不需要关心.

假设我们不知道`Maybe`, `[]`, `IO` 的内部结构, 我们可以对其进行操作吗? 答案是可以. 但是需要该抽象数据类型满足一个条件: 它必须是一个 `Functor`.

    class Functor f where
    fmap :: (a -> b) -> f a -> f b

fmap 这个函数有甚么意义呢? fmap将一个函数从一个域变换到 f (f 可以是 `Maybe`, `[]`, `IO` , ...)域. 比如我们在 Int 上有一个后继数函数 succ. 那么我们就可以将其变换到 Maybe 域:

    fmap succ (Just 1) == Just 2
    fmap succ Nothing == Nothing
    m1 = Just 10
    m2 = Nothing
    fmap succ m1
    fmap succ m2

可以将其变换到List 域:

    fmap succ [1,2,3] == [2,3,4]
    fmap succ [] == []

可以将函数其变换到IO 域:

    fmap reverse getLine

我们完全不用关心m1, m2, Just 10, getLine 的内部结构是怎么样的, 我们就可以对其内容进行操作了.

我们如果有 `Int, Double, Float, String` 上的函数, 通过 fmap , 我们不费吹灰之力就有了 `Maybe Int, Maybe Double, Maybe String, [Int], [Double], [String], IO Int , IO String , Writer w Int, Reader r String, State s Double` ... 上的函数. 这就是抽象的威力.

## 数据的映射 :: Appicative

上面讲到fmap 将函数 `a -> b` 变换成 `f a -> f b`. 假如 `f` 是完全抽象的， 我们怎么的到一个 `f a` 呢? 这就不仅要变换函数, 还要变换数据. 这就是 Applicative class要解决的问题.

    class Functor f => Applicative f where
      pure :: a -> f a
      <*> :: f (a -> b) -> f a -> f b

先来看 pure. pure 可以将一个 a 变换成 `f a` , 例如:

    pure 1 == Just 1
    pure 1 == [1]
    pure "type this string" -- == getLine 输入 "type this string"

函数实际上也是数据，那么 `pure succ`　也是可以的. 但是 pure succ 的类型是 `f (Int -> Int)`. 这个函数我们能拿来干什么呢. 貌似啥都干不了, 因为这个函数被包裹在 f 里面了, 我们不能把他拿出来，不能喂给他一个 Int, 吐出一个Int.

这时候 `<*>` 就很有用了. 它能够将 `f(a -> b)` 这个数据变成一个 `f a -> f b` 的函数. 有了这个函数实际上我们都可以不需要fmap了因为:

pure　把一个函数 `a -> b` 变换成 `f(a -> b)`, `<*>` 把 `f(a -> b)` 变换成 `f a -> f b`. 两个一结合:

    (<*>) . pure :: (a -> b) -> f a -> f b

再回顾下 fmap 的类型:

    fmap :: (a -> b) -> f a -> f b

因此:

    fmap == (<*>) . pure

到此, 我们已经能加任意类型 a, b 以及他们直接的函数 `a -> b` 转换到 f 域上了.

现在我们就可以对任意 f 域里面的数据进行操作了. 例如对 IO a :

    -- t.hs
    import Control.Applicative
    import Data.List(sort)

    (==>) :: Applicative f => f a -> (a -> b) -> f b
    a ==> g = fmap g a

    main= getLine ==> words ==> map read ==> (sort::[Int] -> [Int])

main 函数把 `getLine :: IO String` 变成有序的 `IO [Int]`, 在我们完全不知道 IO a 的内部构造的条件下.

运行一下:

    [root@myhost:tmp]$ ghci t.hs
    GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    [1 of 1] Compiling Main             ( t.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> main
    1 3 2 4
    [1,2,3,4]
    *Main>

## 两个世界之间的联系: Monad

到目前为止, 我们接触到的从世界 a 到 f a 的函数只有 return 一个.

实际上这种转换函数有很多．

假如我们要有很多 `a -> f a` 的函数，　怎么把它复合合起来呢？

假如我们有这个三个函数 检查输入是否是个数，是个非负数，是个偶数 :

    import Control.Applicative

    parseInt :: String -> Maybe Int
    parseInt s = let xs = reads s in if null xs then Nothing else pure.fst.head $ xs

    notNegative :: Int -> Maybe Int
    notNegative i = if i < 0 then Nothing else pure i

    notOdd :: Int -> Maybe Int
    notOdd x = if mod x 2 == 0 then pure x else Nothing

最简单的, 我们采用Applicative 函数去复合他们

    checkArg :: String -> Maybe (Maybe Int)
    checkArg arg= (fmap $ fmap notOdd) $ (fmap notNegative) (parseInt arg)
    -- checkArg "1234" == Just (Just 1234)
    -- checkArg "-1" == Just Nothing
    -- checkArg "abc" == Nothing

问题来了. 不好复用. 多几层检查的话我们的函数签名就要变成 `Maybe (Maybe (Maybe (Maybe (Maybe ...))))` 了, 那就是 Lisp 了. :D

因此我们需要一个把 Maybe (Maybe Int) 变成 Maybe Int 的一个机制. 当当当:

    class Applicative m => Monad m where
      join :: m (m a) -> m a

这就是所谓的 Monad 了.

上面的例子, 就变成这样:

    checkArg2 :: String -> Maybe Int
    checkArg2 = join.fmap notOdd.join.fmap notNegative.parseInt

后来呢, 人们嫌每次函数复合都写 join.fmap 太费劲, 于是就整了个语法糖:

    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    m >>= f = join $ fmap f m

于是

    checkArg3 arg = parseInt arg >>= notNegative >>= notOdd

顺眼多了吧.

## 参考文献

1.  [回albertLee:关于Category Theory 和Monad](http://www.iteye.com/topic/147443)
2.  [Haskell与范畴论](http://yi-programmer.com/2010-04-06_haskell_and_category_translate.html)

