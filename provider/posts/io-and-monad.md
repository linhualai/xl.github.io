---
title: 纯洁的 IO 与 自由的 Monad
published: 2012-08-29
excerpt: 
tags: Haskell, Monad
---

## Myths and Legends

Haskell 里 IO 操作也是纯函数，这多多少少会让初学者感情上难以接受。前几年 TopLanguage 就有几个哥们为 IO 是不是纯的争得面红耳赤，不欢而散。

在 Haskell 里面 IO 操作时是用 Monad 来建模的，一般的 IO 教程上会使用这样的模型来帮助理解：

    data IO a = IO (RealWorld -> (RealWorld, a))

这个模型的好处是易于理解。

## 实现 IO Monad

真正要理解一个概念，最好的办法就是实现它。在实现它的时候，为啥这个概念要这么设计，很多取舍你才会更深刻的理解它。

下面我们自己来实现一个 IO 库好了。

首先是 import, 先用 NoImplicitPrelude 防止自动引入 Prelude 里面预定义的 IO(我们要自己实现).

~~~ {lang="haskell"}

{-# LANGUAGE DeriveFunctor, NoImplicitPrelude #-}
import Data.Function
import Data.Functor
import Data.String
import Control.Monad
import Prelude(Show(..), (++))
import qualified System.IO
import qualified System.Exit

~~~

定义三个基本的 IO 操作: GetLine, PutStr, Stop.

    data BasicIO next = GetLine (String -> next) | PutStr String next | Stop
        deriving (Functor)

定义一个 Helper 函数：

    class FunctorTrans t where
        liftF :: Functor f => f a -> t f a

    instance FunctorTrans Free where
        liftF = Free . fmap Pure

定义三个 IO 函数: putStr, getLine, exit: 这是我们最经常用(如果不是唯一用的)的三个 IO 函数.

    putStr :: String -> IO ()
    putStr s = liftF $ PutStr s ()

    getLine :: IO String
    getLine = liftF $ GetLine id

    exit :: IO a
    exit = liftF Stop

print 和 putStrLn 也是常用的， 当然需要定义一下:

    putStrLn :: String -> IO ()
    putStrLn = putStr . (++ "\n")

    print :: Show a => a -> IO ()
    print = putStrLn . show

那上面的 IO 又是什么呢？ 下面我们就来定义一下：

    data Free f a = Pure a | Free (f (Free f a)) deriving (Functor)
    type IO = Free BasicIO

IO 是一个 Monad, 我们当然要实现：

    instance Functor f => Monad (Free f) where
        return = Pure
        Pure a >>= f = f a
        (Free x) >>= f = Free $ fmap (>>= f) x

现在这个 IO 就可用了，不信的话，下面用上面定义的 IO 来写一段代码吧：

    freeMain = do
        x <- getLine
        y <- getLine
        putStrLn $ x ++ y
        exit
        putStrLn $ x ++ y

到此为止，我们定义的 IO， 就算写完了。 这个 IO 是纯洁的吗？是。至少上面我们用到的都是纯函数。

那 IO Monad 真的是纯的吗？ 下面把我们自己当做 Haskell 的运行时。看一下 IO Monad 到底是怎么运行的：

    runIO :: IO a -> System.IO.IO a
    runIO (Pure a) = return a
    runIO (Free (GetLine f)) = System.IO.getLine >>= runIO . f
    runIO (Free (PutStr s next)) = System.IO.putStr s >> runIO next
    runIO (Free Stop) = System.Exit.exitSuccess

给个 main 函数运行一下：

    main = runIO freeMain

## 小结

Monad 是纯洁的吗？ 虽然在 IO Monad 执行的时候会用到不纯的 Syste.IO 里面的函数, 我们应该还是应该把 IO Monad 看成纯函数。 不能因为 runIO 的不纯就说 IO Monad 不纯。这件事我们应该从语义上去看，不应该从实现的手段去看。 比如我们用 C 语言写一个 Haskell 的解释器，C 语言里面都是副作用，那么在这个解释器里运行的 Haskell 代码是不纯的吗？

## 从控制反转的角度来看 IO Monad

上面的代码, 虽然用在 Haskell 中用纯函数实现了一个 IO Monad。但是还是不太直观。换个角度来看： 我们的 IO Monad 只是提供了一些 IO 操作的步骤，并没有真正的进行 IO，真正的 IO 操作是在 runIO 里面才被执行的。这个有点 IOC 的意思，熟悉数据库的老大可能就笑了，这个 IO Monad 不就是的 redo log 或 commit log 吗。先记下操作步骤，具体把数据写入 B-Tree 是等另一个进程 (runIO) 去做的。

## 一点说明

上面的 Free Monad 是代数中的[Free Structure](http://www.haskell.org/haskellwiki/Free_structure) 在 Haskell 中的对应物。其实简单的理解 Free BasicIO 就是 BasicIO 的一个列表 : `[BasicIO]`.

PS: Haskell 里面的这伙人真是变态，一个个仗着自己的医生(Doctor)头衔，写个代码都是各种抽象代数，范畴学的，绕到云里雾里。 最让人生气的是还能运行。。。

## 参考文献

1.[Haskell for all: Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
