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

抽象的に捉えると、しゃくとり法は「区間に対して定まるなんらかの値 $f$（合計、長さなど）」と「現在の右端を指すポインタ」を状態として持ちながらリストを走査し、$f$ のリストを出力するアルゴリズムと言えます。

Haskell には `mapAccumL` という、まさに「状態を持ちながらリストを走査し、リストを出力する」ための関数があるのでこれを利用するとすっきり書けそうです。

これを踏まえて書いてみたのが以下の関数です。条件を満たす最小の右端を求めたい場合と最大の右端を求めたい場合があるので、2種類用意しています。

```haskell
-- cond を満たすまで区間を伸ばす。(累積値, 長さ)のリストを返す。
shakutori :: (a -> Bool) -> (a -> b -> a) -> (a -> b -> a) -> a -> [b] -> [(a, Int)]
shakutori cond op invOp initial as = snd $ mapAccumL f ((initial, 0), as) as
  where
    f ((acc, len), r : rs) x
      | cond acc = (((acc `invOp` x, len - 1), r : rs), (acc, len))
      | otherwise = f ((acc `op` r, len + 1), rs) x
    f ((acc, len), []) x = (((acc `invOp` x, len - 1), []), (acc, len + if cond acc then 0 else 1))

-- cond を満たす間、区間を伸ばす。(累積値, 長さ)のリストを返す。
shakutori' :: (a -> b -> Bool) -> (a -> b -> a) -> (a -> b -> a) -> a -> [b] -> [(a, Int)]
shakutori' cond op invOp initial as = snd $ mapAccumL f ((initial, 0), as) as
  where
    f ((_, 0), r : rs) x
      | cond initial r = f ((initial `op` r, 1), rs) x
      | otherwise = (((initial, 0), rs), (initial, 0))
    f ((acc, len), r : rs) x
      | cond acc r = f ((acc `op` r, len + 1), rs) x
      | otherwise = (((acc `invOp` x, len - 1), r : rs), (acc, len))
    f ((acc, len), []) x = (((acc `invOp` x, len - 1), []), (acc, len))

```

この関数を用いていくつか問題を解いてみます。

## [ABC022 B - 細長いお菓子](https://atcoder.jp/contests/arc022/tasks/arc022_2)

```haskell
main :: IO ()
main = do
  n <- getInt
  as <- getInts

  let ans = maximum $ map snd $ shakutori' (flip IntSet.notMember) (flip IntSet.insert) (flip IntSet.delete) IntSet.empty as

  print ans
```

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

  let res = shakutori' (\acc x -> acc * x <= k) (*) div (1 :: Int) as
      ans = maximumDef 0 $ map snd res
  print ans
```

## [ABC038 C - 単調増加](https://atcoder.jp/contests/abc038/tasks/abc038_c)

```haskell
main :: IO ()
main = do
  n <- getInt
  as <- getInts

  let ans = sum $ map snd $ shakutori' (<) (\_ x -> x) const 0 as

  print ans
```

## [ABC098 D - Xor Sum 2](https://atcoder.jp/contests/abc098/tasks/arc098_b)

```haskell
main :: IO ()
main = do
  n <- getInt
  as <- getInts

  let res = shakutori' (\(acc, acc') x -> acc + x == acc' .^. x) op invOp (0 :: Int, 0 :: Int) as
        where
          op (acc, acc') x = (acc + x, acc' .^. x)
          invOp (acc, acc') x = (acc - x, acc' .^. x)
      ans = sum $ map snd res
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
main :: IO ()
main = do
  (n, a, b) <- getInt3
  s <- BS.unpack <$> BS.getLine

  let as = map (\ch -> if ch == 'a' then 1 else 0) s
      bs = map (\ch -> if ch == 'b' then 1 else 0) s
      ls = map snd $ shakutori (>= a) (+) (-) (0 :: Int) as
      rs = map snd $ shakutori (>= b) (+) (-) (0 :: Int) bs
      ans = sum $ zipWith (\l r -> max (r - l) 0) ls rs
  print ans
```
