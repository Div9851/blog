---
title: "AtCoder部 2026-01-14"
date: 2026-01-14
tags: ["AtCoder部"]
---

久しぶりに Haskell で解いてみました。慣れなくて時間がかかりました。

## [ABC426 D - Pop and Insert](https://atcoder.jp/contests/abc426/tasks/abc426_d)

全て0にするときの最小コストを考える（0と1を反転して解けば1にするときの最小コストも求められるので、0にするときだけ考えればok）。

全ての1を、ある位置より左にあるグループと右にあるグループに分けて、左にあるグループは左からの操作で、右にあるグループは右からの操作で1にすることにする。
このときのコストは、1の個数+無駄に操作しないといけない0の個数×2となる。
分け方を全て試して、最小値を求めればよい。

```haskell
solve' :: Int -> [Char] -> Int
solve' n s =
  let countZero acc c = acc + if c == '0' then 1 else 0
      ones = 0 : (map (+ 1) . elemIndices '1') s ++ [n + 1]
      cumL = IA.listArray @UArray (0, n) $ scanl' countZero (0 :: Int) s
      cumR = IA.listArray @UArray (1, n + 1) $ scanr (flip countZero) (0 :: Int) s
   in minimum $ zipWith (\cur nxt -> cur + (n + 1 - nxt) + cumL IA.! cur + cumR IA.! nxt) ones (drop 1 ones)

solve :: IO Int
solve = do
  n <- readInt <$> BS.getLine
  s <- BS.unpack <$> BS.getLine

  let rev c = if c == '0' then '1' else '0'

  return $ min (solve' n s) (solve' n (map rev s))

main :: IO ()
main = do
  t <- readInt <$> BS.getLine
  ans <- replicateM t solve
  putStr $ unlines $ map show ans
```

## [ABC427 C - Bipartize](https://atcoder.jp/contests/abc427/tasks/abc427_c)

頂点を2つのグループに分ける方法を全て試せばok。

```haskell
main :: IO ()
main = do
  (n, m) <- readInt2 <$> BS.getLine
  es <- replicateM m (readInt2 <$> BS.getLine)

  let ans = minimum $ map (f . IA.listArray @UArray (1, n)) $ replicateM n [0 :: Int, 1 :: Int] :: Int
        where
          f colors = sum $ map (\(u, v) -> if colors IA.! u == colors IA.! v then 1 else 0) es

  print ans
```

## [ABC428 C - Brackets Stack Query](https://atcoder.jp/contests/abc428/tasks/abc428_c)

「正しいカッコ列である」は「`(` を +1、`)` を -1 として累積和を取ったとき、常に0以上かつ末尾が0」と言い換えられる。

（累積和の列, 負の要素の個数）を状態として、クエリごとに適切に状態遷移させていけばよい。

```haskell
main :: IO ()
main = do
  q <- readInt <$> BS.getLine
  qs <- replicateM q (words . BS.unpack <$> BS.getLine)

  let ans = drop 1 $ scanl' f ([0], 0) qs
        where
          f (x : xs, cnt) query = case query of
            ["1", c] ->
              let acc = x + if c == "(" then 1 else -1
                  cnt' = cnt + if acc < 0 then 1 else 0
               in (acc : x : xs, cnt')
            ["2"] ->
              let cnt' = cnt - if x < 0 then 1 else 0
               in (xs, cnt')
  putStr <$> unlines $ map (yesno . \(x : xs, cnt) -> x == 0 && cnt == 0) ans
```
