---
title: "AtCoder部 2026-01-218"
date: 2026-01-28
tags: ["AtCoder部"]
---

## [ABC427 D - The Simple Game](https://atcoder.jp/contests/abc427/tasks/abc427_d)

ゲーム問題では、最終的な盤面から遡って、各盤面が先手必勝か後手必勝か判定していくのが定石。

まず「最終的に頂点 $v$ にいるとき、その盤面は先手必勝か後手必勝か」を考えると、頂点 $v$ に A が書かれていれば先手必勝、頂点 $v$ に B が書かれていれば後手必勝。

次に、「$i$ 手前の時点で頂点 $v$ にいるとき、その盤面は手番プレイヤーの必勝かどうか」が求まっているとして、「$i+1$ 手前の時点で頂点 $v$ にいるとき、その盤面は手番プレイヤーの必勝かどうか」を求めることを考える。
これは、遷移先に一つでも後手必勝盤面があるなら今の盤面は先手必勝、遷移先が全て先手必勝盤面なら今の盤面は後手必勝とすることで求められる。

これを繰り返して「$2K$ 手前の時点で頂点 $v$ にいるとき、その盤面は手番プレイヤーの必勝かどうか」を求めればよい。

```haskell
solve :: IO String
solve = do
  (n, m, k) <- getInt3
  s <- BS.unpack <$> BS.getLine
  uvs <- replicateM m getInt2

  let initVal = IA.listArray @UArray @Int (1, n) $ map (\ch -> if ch == 'A' then 1 else -1) s
      step as _ = IA.accumArray @UArray max (-1) (1, n) $ map (\(u, v) -> (u, -(as IA.! v))) uvs
      ans = foldl step initVal [1 .. 2 * k]

  return $ if ans IA.! 1 == 1 then "Alice" else "Bob"

main :: IO ()
main = do
  t <- getInt
  ans <- replicateM t solve
  putStr $ unlines ans
```

## [ABC431 D - Robot Customize](https://atcoder.jp/contests/abc431/tasks/abc431_d)

ぱっと見（頭の重さの合計, 体の重さの合計）を状態として、各状態のあり得る嬉しさの最大値を求める DP をしたくなる。ただ、これだと状態が多くて間に合わない。差にしか興味ないので（体の重さの合計-頭の重さの合計）を状態とすればよい。

```haskell
main :: IO ()
main = do
  n <- getInt
  parts <- replicateM n getInt3

  let sumW = sum $ map (\(w, _, _) -> w) parts
      bnd = (-sumW, sumW)
      initVal = IA.accumArray @UArray @Int max minBound bnd [(0, 0)]
      step as (w, h, b) =
        IA.accumArray @UArray @Int max minBound bnd $
          concatMap (\(d, s) -> if s == minBound then [] else [(d - w, s + h), (d + w, s + b)]) (IA.assocs as)
      dp = foldl step initVal parts
      ans = maximum $ [s | (d, s) <- IA.assocs dp, d >= 0]

  print ans
```

## [ABC432 C - Candy Tribulation](https://atcoder.jp/contests/abc432/tasks/abc432_c)

C 問題にしたら難しい...。

$A$ はソートされているとしてよいのでソートする。

人 $i$ に配る大きい飴の個数を $w_i$ とする。受け取る飴の重さの合計が等しいことから、$Yw_1+X(A_1-w_1)=Yw_k+X(A_k-w_k)$ とならなければならない。これを変形していくと、$w_k=w_1-X(A_k-A_1)/(Y-X)$ が得られる。$X(A_k-A_1)/(Y-X)$ は定数なので、結局、$w_1$ を定めると $w$ が一意に定まるとわかる。

あとは、全ての $k$ について $0 \le w_k \le A_k$ を満たすように $w_1$ を定めるときの、$w_1 + w_2 + \dots + w_N$ の最大値を求める問題になる。

$0 \le w_k$ かつ $w_1 \le A_1$ から、全ての $k$ について $X(A_k-A_1)/(Y-X) \le A_1$ でないといけない（これを満たさない場合は解なし）。

全ての $k$ について $X(A_k-A_1)/(Y-X) \le A_1$ である場合、$w_1=A_1$ で最大値を取る。

```haskell
main :: IO ()
main = do
  (n, x, y) <- getInt3
  as <- getInts

  let as' = sort as
      a1 = head as'

  when (any (\a -> x * (a - a1) `mod` (y - x) /= 0 || x * (a - a1) > (y - x) * a1) as') $ do
    print (-1)
    exitSuccess

  print $ sum $ map (\a -> a1 - x * (a - a1) `div` (y - x)) as'
```
