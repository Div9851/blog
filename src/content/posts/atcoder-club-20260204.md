---
title: "AtCoder部 2026-02-04"
date: 2026-02-04
tags: ["AtCoder部"]
---

## [ABC430 D - Neighbor Distance](https://atcoder.jp/contests/abc430/tasks/abc430_d)

新しい人が来たとき、影響を受けるのはその両隣になる人のみ。差分更新していけばいい。

```haskell
main :: IO ()
main = do
  n <- getInt
  xs <- getInts

  let im0 = IntMap.fromList [(0, maxBound :: Int), (maxBound, 0)]
      ans = drop 1 $ map snd $ scanl f (im0, maxBound) xs
        where
          f (im, acc) x =
            let (l, dl) = fromJust $ IntMap.lookupLT x im
                (r, dr) = fromJust $ IntMap.lookupGT x im
                dl' = min (x - l) dl
                dr' = min (r - x) dr
                dx = min (x - l) (r - x)
                im' = IntMap.insert x dx $ IntMap.insert r dr' $ IntMap.insert l dl' im
                acc' = acc - dl + dl' - dr + dr' + dx
             in (im', acc')

  putStr $ unlines $ map show ans
```

## [ABC433 D - 183183](https://atcoder.jp/contests/abc433/tasks/abc433_d)

各 $0 \le k \le 10$, $0 \le r \le M-1$ について $10^kA_i \mod m = r$ を満たす $i$ の個数を数えておけば、各 $i$ について $f(A_j, A_i) \mod M = 0$ となる $j$ の個数が簡単に求められる。

```haskell
main :: IO ()
main = do
  (n, m) <- getInt2
  as <- getInts

  let cnt = IA.listArray @Array (0, 10) $ [IntMap.fromListWith (+) [(a `mod` m * 10 ^ k `mod` m `mod` m, 1 :: Int) | a <- as] | k <- [0 :: Int .. 10]]
      ans = sum $ map (\a -> fromMaybe 0 $ IntMap.lookup ((m - a `mod` m) `mod` m) $ cnt IA.! length (show a)) as
  print ans
```

## [ABC436 D - Teleport Maze](https://atcoder.jp/contests/abc436/tasks/abc436_d)

BFS をする。ただし、毎回ワープの遷移をしていると間に合わない。よく考えると、ワープの遷移をするのは、最初に各タイプのワープポイントに到達したときのみとわかる。

Haskell での提出はTLE 😭

```haskell
main :: IO ()
main = do
  (h, w) <- getInt2
  s <- replicateM h BS.getLine

  let bnds = ((1, 1), (h, w))
      s' = IA.listArray @UArray @Char bnds $ BS.unpack $ BS.concat s
      warp = IA.accumArray @Array (flip (:)) [] ('a', 'z') $ [(ch, v) | v <- range bnds, let ch = s' IA.! v, inRange ('a', 'z') ch]

  dist <- newArray @IOUArray bnds (-1 :: Int)
  queue <- Queue.new (rangeSize bnds)
  seen <- newArray @IOUArray ('a', 'z') False

  writeArray dist (1 :: Int, 1 :: Int) 0
  Queue.pushBack queue (1 :: Int, 1 :: Int)

  let around4 (r, c) = [v | v <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)], inRange bnds v, s' IA.! v /= '#']
      loop = do
        entry <- Queue.popFront queue

        case entry of
          Nothing -> return ()
          Just v -> do
            d <- readArray dist v

            forM_ (around4 v) $ \u -> do
              d' <- readArray dist u
              when (d' == -1) $ do
                writeArray dist u (d + 1)
                Queue.pushBack queue u

            let ch = s' IA.! v
            isSeen <- if inRange ('a', 'z') ch then readArray seen ch else return False

            when (inRange ('a', 'z') ch && not isSeen) $ do
              forM_ (warp IA.! ch) $ \u -> do
                d' <- readArray dist u
                when (d' == -1) $ do
                  writeArray dist u (d + 1)
                  Queue.pushBack queue u

              writeArray seen ch True

            loop

  loop

  ans <- readArray dist (h, w)
  print ans
```

TLEとれず C++ で提出

```cpp
int main() {
    int H, W;
    cin >> H >> W;
    vector<string> S(H);
    vector warp(26, vector<PI>());
    for (int i = 0; i < H; i++) {
        cin >> S[i];
        for (int j = 0; j < W; j++) {
            if (islower(S[i][j])) warp[S[i][j] - 'a'].emplace_back(i, j);
        }
    }
    vector dist(H, vector<int>(W, -1));
    vector<int> seen(26);
    queue<PI> que;
    dist[0][0] = 0;
    que.emplace(0, 0);
    while (!que.empty()) {
        auto [y, x] = que.front();
        que.pop();
        for (int i = 0; i < 4; i++) {
            int nx = x + dx[i], ny = y + dy[i];
            if (0 > nx || nx >= W || 0 > ny || ny >= H || S[ny][nx] == '#' || dist[ny][nx] != -1) continue;
            dist[ny][nx] = dist[y][x] + 1;
            que.emplace(ny, nx);
        }
        if (islower(S[y][x]) && !seen[S[y][x] - 'a']) {
            for (auto [ny, nx] : warp[S[y][x] - 'a']) {
                if (dist[ny][nx] != -1) continue;
                dist[ny][nx] = dist[y][x] + 1;
                que.emplace(ny, nx);
            }
            seen[S[y][x] - 'a'] = true;
        }
    }
    cout << dist[H - 1][W - 1] << NL;
}
```
