---
title: 在 Haskell 中使用 Continuation 实现单线程并发控制
published: 2012-08-23
excerpt: 
tags: Haskell, Scheme
---

之前在 [Section 3.3. Continuations](http://scheme.com/tspl4/further.html#./further:h3) 看到 Scheme 实现的一个用 call/cc 模拟的多任务程序, 还挺好玩的:

~~~ {lang="scheme"}

    (define lwp-list '())
    (define lwp
      (lambda (thunk)
        (set! lwp-list (append lwp-list (list thunk)))))

    (define start
      (lambda ()
        (let ([p (car lwp-list)])
          (set! lwp-list (cdr lwp-list))
          (p))))

    (define pause
      (lambda ()
        (call/cc
          (lambda (k)
            (lwp (lambda () (k #f)))
            (start)))))

    (lwp (lambda () (let f () (pause) (display "h") (f))))
    (lwp (lambda () (let f () (pause) (display "e") (f))))
    (lwp (lambda () (let f () (pause) (display "y") (f))))
    (lwp (lambda () (let f () (pause) (display "!") (f))))
    (lwp (lambda () (let f () (pause) (newline) (f))))
    (start)

~~~

这个程序会不断地输出: \>hey! \>hey! \>hey! \>hey! \>...

## 热身

我估摸着吧, Haskell 里的 callCC 也可以这么玩，不过 Haskell 里面没有副作用，也就没有赋值，这个 `set!` 就需要绕个弯子, 借助 IORef 来实现了。

~~~ {lang="haskell"}

import Control.Monad.Cont
import Data.IORef

main = (`runContT` return) $ do
    ref <- lift $ newIORef undefined
    callCC $ \k -> lift $ writeIORef ref k
    lift $ putStrLn "print once"
    continue <- lift $ readIORef ref
    continue ()

~~~

稍微解释一下：这个程序会不断地打印 "print once". 程序的执行过程是这样的，callCC 把 callCC 之后的三行代码绑定到 k 上了， 然后把 k 保存到 ref 里面。 这后三行代码呢，打印一个行 "print once", 然后把保存在 ref 里的 k 拿出来，继续执行, 跳转到 callCC 那去了。 这样就形成了一个死循环，不断打印。

## 模拟

接下来就简单了，依葫芦画瓢抡一个 Haskell 版本的 light weight process 了 :

~~~ {lang="haskell"}

    import Control.Monad.Cont
    import Data.IORef

    pause ref = callCC $ \k -> do
        lwp ref (k ())
        start ref

    start ref = do
        (t:ts) <- lift $ readIORef ref
        lift $ writeIORef ref ts
        t

    lwp ref t = lift $ modifyIORef ref $ \ts -> ts ++ [t]

    main = (`runContT` return) $ do
        ref <- lift $ newIORef []
        lwp ref . forever $ do
            pause ref
            lift $ putStrLn "Hello"
        lwp ref . forever $ do
            pause ref
            lift $ putStrLn "World"
        start ref
~~~

这个程序呢，不断打印 "Hello World", 几乎就是原来 Scheme 版本的一一对应， 没啥好解释的了。

## 后话

ps: The Scheme Programming Language 真是一本好书。 前三章就把 Scheme 编程的要素讲完了， 包括 Continuation, CPS, 宏定义等都讲透了， 后面九章就是具体解释和应用了。Kent Dybvig 端的是功力深厚，学术一流，工程能力也没话说, Chez petite 编译器不管易用性，性能都是一等一的。话说作者还把这本书公开放在网上了: http://www.scheme.com/tspl4/ , 真的是功德无量哈。SICP 固然是好书，不过里面讲 Scheme 的东西其实不多，看过 SICP 的来看这本，想必会如或至宝。SICP 练就一身内力，不过不好使出来 (小测验: 用 Scheme 编写一个文件读写的程序?), 这本 TSPL 就是教你太极招式的好书。

## 参考文献

1.  [The Scheme Programming Language](http://scheme.com/tspl4)

