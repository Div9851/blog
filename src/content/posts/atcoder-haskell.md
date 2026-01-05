---
title: "Haskell で AtCoder をするための環境を作る"
date: 2026-01-06
tags: ["競技プログラミング"]
---

Haskell で AtCoder をするための環境を作り直したので、わすれないように手順をメモ。

必要なツール

- GHCup: GHC, cabal を管理するツール
- GHC: コンパイラ
- Cabal: パッケージマネージャ

まず GHCup をインストールし、GHCup で GHC と cabal をインストールする。2026年1月現在、AtCoder 環境では GHC 9.8.4, Cabal 3.14 を利用しているようなので、合わせておくのが無難そう。

これらのツールがインストールできたら、 [Div9851/atcoder-haskell](https://github.com/Div9851/atcoder-haskell/tree/71d12ffa77cb2603d4189bf340b1c262cd914e4a) を clone してきて、README に書いてあるコマンドを実行してパッケージをインストールすれば ok。

問題なくインストールできていれば `cabal run` で `App/Main.hs` が実行できるはず。
