---
title: "Haskell でのしゃくとり法の書き方"
date: 2026-01-21
tags: ["競技プログラミング"]
---

正の整数からなる、長さ $N$ の数列が与えられる。和が $K$ 以上となる区間の個数を求めよ。

という問題の解き方を考えます。

例えば、区間 [1,3] が条件を満たす場合、区間 [1,4] や [1,5] も条件を満たします。よって、各要素を区間の左端にした場合の、条件を満たす最小の右端が求まればよいです。

まず、1番目の要素を左端にする場合、条件を満たす最小の右端は、和が $K$ 以上となるまで右端をずらしていくことで求められます。

次に、2番目の要素を左端にする場合の最小の右端を求めます。よく考えるとこの場合の答えが先ほど求めた答えより小さくなることはあり得ません。よって、続きから走査を再開すればよいです。

これを繰り返すことで、計算量 $O(N)$ で全ての場合の答えを求めることができます。

...というのがしゃくとり法です。もっと詳しく知りたい人は→[しゃくとり法 (尺取り法) の解説と、それを用いる問題のまとめ](https://qiita.com/drken/items/ecd1a472d3a0e7db8dce)

さて、しゃくとり法を Haskell でスマートに書くにはどうしたらよいでしょうか？

抽象的に捉えると、しゃくとり法は「区間に対して定まるなんらかの値 $f$（合計、合計と長さのペアなど）」と「現在の右端を指すポインタ」を状態として持ちながらリストを走査し、$f$ のリストを出力するアルゴリズムと言えます。

Haskell には `mapAccumL` という、まさに「状態を持ちながらリストを走査し、リストを出力する」ための関数があるのでこれを利用するとすっきり書けそうです。

これを踏まえて書いてみたのが以下の関数です。

```haskell
-- cond を満たすか末尾に達するまで区間を伸ばす。(区間に対して定まる値, cond を満たすかどうか)のリストを返す。
shakutori :: (a -> Bool) -> (a -> b -> a) -> (a -> b -> a) -> a -> [b] -> [(a, Bool)]
shakutori cond op invOp initial as = snd $ mapAccumL f (initial, as) as
  where
    f (acc, r : rs) x
      | cond acc = ((acc `invOp` x, r : rs), (acc, True))
      | otherwise = f (acc `op` r, rs) x
    f (acc, []) x = ((acc `invOp` x, []), (acc, cond acc))
```

この関数を用いていくつか問題を解いてみます。

## [ABC032 C - 列](https://atcoder.jp/contests/abc032/tasks/abc032_c)

```haskell
main :: IO ()
main = do
  (n, k) <- getInt2
  as <- replicateM n getInt

  when (0 `elem` as) $ do
    print n
    exitSuccess

  when (k == 0) $ do
    print 0
    exitSuccess

  let res = shakutori ((> k) . fst) op invOp (1 :: Int, 0 :: Int) as
        where
          op (acc, len) x = (acc * x, len + 1)
          invOp (acc, len) x = (acc `div` x, len - 1)
      ans = maximumDef 0 $ map (\((_, len), ok) -> if ok then len - 1 else len) res
  print ans
```

## [ABC098 D - Xor Sum 2](https://atcoder.jp/contests/abc098/tasks/arc098_b)

```haskell
main :: IO ()
main = do
  n <- getInt
  as <- getInts

  let res = shakutori (\(acc, acc', _) -> acc /= acc') op invOp (0 :: Int, 0 :: Int, 0 :: Int) as
        where
          op (acc, acc', len) x = (acc + x, acc' .^. x, len + 1)
          invOp (acc, acc', len) x = (acc - x, acc' .^. x, len - 1)
      ans = sum $ map (\((_, _, len), ok) -> if ok then len - 1 else len) res
  print ans
```

## [ABC429 D - On AtCoder Conference](https://atcoder.jp/contests/abc429/tasks/abc429_d)

```haskell
main :: IO ()
main = do
  (n, m, c) <- getInt3
  as <- getInts

  let k = length $ nubOrd' as
      as' = as ++ map (+ m) as
      as'' = IntMap.toAscList $ IntMap.fromListWith (+) [(a, 1 :: Int) | a <- as']
      pos = map fst as''
      cnt = map snd as''
      xs = map fst $ shakutori (>= c) (+) (-) 0 cnt
      ans = sum $ zipWith (*) xs (zipWith (-) (drop k pos) (drop (k - 1) pos))
  print ans
```

## [ABC430 C - Truck Driver](http://atcoder.jp/contests/abc430/tasks/abc430_c)

```haskell
main = do
  (n, a, b) <- getInt3
  s <- BS.unpack <$> BS.getLine

  let as = map (\ch -> if ch == 'a' then 1 else 0) s
      bs = map (\ch -> if ch == 'b' then 1 else 0) s
      op (cnt, len) x = (cnt + x, len + 1)
      invOp (cnt, len) x = (cnt - x, len - 1)
      ansA = shakutori ((>= a) . fst) op invOp (0 :: Int, 0 :: Int) as
      ansB = shakutori ((>= b) . fst) op invOp (0 :: Int, 0 :: Int) bs
      ans = sum $ zipWith f ansA ansB
        where
          f (_, False) _ = 0
          f ((_, len), True) ((_, len'), False) = (len' + 1) - len
          f ((_, len), True) ((_, len'), True) = max (len' - len) 0
  print ans
```
