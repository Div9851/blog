---
title: "AtCoder部 2026-01-21"
date: 2026-01-21
tags: ["AtCoder部"]
---

たまたましゃくとり法の問題が多く、Haskell で綺麗に書く方法を考える良い機会になりました。

## [ABC429 D - On AtCoder Conference](https://atcoder.jp/contests/abc429/tasks/abc429_d)

$M$ が大きいので工夫が必要。人が1人以上いる点の個数を $k$、座標を昇順に $P_1, P_2, \dots, P_k$ としたとき、各 $i$ について、$P_i$ と $P_{i+1}$ の間のどの点からスタートしても高橋くんが止まるまでに出会う人数 $X$ は同じになる。この事実を利用すると、計算すべき値は $X_{P_1}, X_{P_2}, \dots, X_{P_k}$ に絞られる。

スタートする点を $P_1+0.5, P_2+0.5, \dots, P_k+0.5$ と時計回りにずらしていったとき、高橋くんが止まる点も時計回りにずれていく（戻ることはない）。よって、しゃくとり法により $O(k)$ で $X_{P_1}, X_{P_2}, \dots, X_{P_k}$ を計算できる。

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
      xs = snd $ mapAccumL f (0 :: Int, cnt) (take k cnt)
        where
          f (acc, r : rs) x
            | acc < c = f (acc + r, rs) x
            | otherwise = ((acc - x, r : rs), acc)
      ans = sum $ zipWith (*) xs (zipWith (-) (drop k pos) (drop (k - 1) pos))
  print ans
```

## [ABC430 C - Truck Driver](http://atcoder.jp/contests/abc430/tasks/abc430_c)

区間の左端を $l$ としたとき、「区間内に a が $A$ 個以上含まれるためには右端がいくつ以上でないといけないか（$L_l$）」「区間内に b が $B$ 個未満含まれるためには右端がいくつ未満でないといけないか（$R_l$）」が求まればよい。

左端を右にずらしていったとき、$L_l$ と $R_l$ も右にずれていく。よって、しゃくとり法が適用できる。

```haskell
main :: IO ()
main = do
  (n, a, b) <- getInt3
  s <- BS.unpack <$> BS.getLine

  let as = map (\ch -> if ch == 'a' then 1 else 0) s
      bs = map (\ch -> if ch == 'b' then 1 else 0) s
      step check (acc, len, r : rs) x
        | check acc = step check (acc + r, len + 1, rs) x
        | otherwise = ((acc - x, len - 1, r : rs), len)
      step check (acc, len, []) x = ((acc - x, len - 1, []), if check acc then len + 1 else len)
      ansA = snd $ mapAccumL (step (< a)) (0, 0, as) as :: [Int]
      ansB = snd $ mapAccumL (step (< b)) (0, 0, bs) bs :: [Int]
      ans = sum $ zipWith (\l r -> max (r - l) 0) ansA ansB
  print ans
```

## [ABC409 C - Equilateral Triangle](https://atcoder.jp/contests/abc409/tasks/abc409_c)

正三角形になるためには、各頂点が $L/3$ ずつ離れていないといけない。よって、$L$ が3の倍数でない場合、正三角形になる頂点の組は存在しない。以下、$L$ は3の倍数とする。

各座標に点が何個あるかを数えておけば、各点についてそれより時計回りに $L/3$ 離れた点の個数、反時計回りに $L/3$ 離れた点の個数を数えるのは容易。これらを掛け算して和をとる。同じ正三角形を3回数えてしまうので、3で割る必要がある。

```haskell
main :: IO ()
main = do
  (n, l) <- getInt2
  ds <- getInts

  when (l `mod` 3 /= 0) $ do
    print 0
    exitSuccess

  let side = l `div` 3
      xs = scanl (\acc x -> (acc + x) `mod` l) 0 ds
      cnt = IA.accumArray @UArray (+) 0 (0, l - 1) [(x, 1 :: Int) | x <- xs]
      ans = (`div` 3) . sum . map (\x -> cnt IA.! ((x - side) `mod` l) * cnt IA.! ((x + side) `mod` l)) $ xs

  print ans
```
