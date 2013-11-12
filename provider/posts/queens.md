---
title: 8 皇后的 Haskell 解法
published: 2012-08-07
excerpt: 
tags: Haskell,Algorithm
---

八皇后谜题问的是怎样将八个皇后摆在国际象棋棋盘上. 使得任意一个皇后都不能攻击另一个皇后(也就是说, 任意两个皇后都不在同一行、同一列或者同一对角线上).

## 前人的解法

albertlee 大牛的Haskell解法：

~~~ {lang="haskell"}

import Control.Monad
import Control.Monad.Writer
import Data.List
diagonal (x1,y1) (x2,y2) = x1 + y1 == x2 + y2
                        || x1 - y1 == x2 - y2
nqueens n = execWriter $ f [1..n] 1 []
    where f [] _ ps = tell [ps]
          f cs r ps = forM_ cs $ \c ->
                          unless (any (diagonal (r,c)) ps) $
                              f (delete c cs) (r + 1) ((r,c):ps)
main = print $ nqueens 4

~~~

-   [haskell求解n皇后问题](http://fleurer-lee.com/2009/04/03/haskellqiu-jie-nhuang-hou-wen-ti.html) : fleuria 大牛的解法
-   [用 Python 秒掉八皇后问题！](http://www.iteye.com/topic/106747) : 众多大牛的解法.

## 有没有简单点的

我愚钝，没看懂。只好自己也想一个解法：

    import Data.List
    queens n = filter valid $ map (zip ([1..n])) $ permutations [1..n]
    valid [] = True
    valid ((a,b):xs) = all (\(x,y) -> abs(a-x) /= abs(b-y)) xs && valid xs

    --6 皇后
    *Main> queens 6
    [[(1,2),(2,4),(3,6),(4,1),(5,3),(6,5)],[(1,5),(2,3),(3,1),(4,6),(5,4),(6,2)],[(1,3),(2,6),(3,2),(4,5),(5,1),(6,4)],[(1,4),(2,1),(3,5),(4,2),(5,6),(6,3)]]

运行的还是挺慢的. 不过代码应该很好懂, 我再解释一下: `map (zip ([1..n])) $ permutations [1..n]`生成所有皇后可能的排列, 用`filter valid`筛选出皇后之间没有冲突的就行了.

## 参考文献
