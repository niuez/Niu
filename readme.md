# Niu Language

Niu言語は, 競技プログラミングにおけるライブラリ制作のための言語です. 開発段階なので注意してください.

## Playground

[Niu Playground](https://niuez.github.io/NiuPlayground/)

## 背景

競技プログラミングではC++を使う人口が多いです. 理由としては, 

- 競技中は型について気を配る時間がなく, 暗黙の型変換で済んでしまう
- 破壊的代入をしやすい
- 標準ライブラリが豊富
- それなりの実行速度がある

また, 競技プログラミングでもC++に代わる言語としてRustが注目され始めています. 
こちらは競技プログラミングの中でもライブラリ(コンテスト前にあらかじめ用意しておく, よく使うデータ構造・アルゴリズムを実装しておいたもの)の制作に向いています.

- トレイト制約による可読性のあるコードやコンパイルエラー表示
- デフォルトでmoveするので, 気づかないデータのコピーが起こらない
- ライフタイムによる参照切れのチェック
- enum, matchなどの構文
- ブロック文が値を返すことができる
- C++と同等の実行速度

使いたい言語がふたつあると, コンテスト用と観賞用の2種類のライブラリを書かなければいけません.  
「ライブラリはRustで書きたいけど, 競技プログラミングのコンテストはC++で出たい」これを解決したいと思い, 開発し始めたのがNiu言語です.  

- ライブラリ制作時は, Niu言語を使ってデータ構造・アルゴリズムを実装
- コンテスト前にライブラリをC++にトランスパイル
- コンテスト中はコピペで利用

が目標です.

## インストール

```sh
git clone https://github.com/niuez/Niu
cd Niu
cargo install --path .
niu help
```

## トランスパイル

```sh
niu trans main.niu
# ライブラリのインポート先のディレクトリを指定
NIU_IMPORT_PATH="path/to/library;path/to/another/library/;" niu trans main.niu 
```

環境変数`NIU_IMPORT_PATH`でインポートするライブラリのディレクトリを設定できます. このリポジトリの`lib/`を指定すると良いです.

## ユニットテスト

[library-checker-problems](https://github.com/yosupo06/library-checker-problems)を利用してユニットテストができます. [ユニットテスト](./doc/test.md)を参考にしてください.


## 言語仕様

- [基本的な部分(Rustとほぼ同じ)](./doc/base.md)
- [C++のコードをNiu言語で利用する](./doc/cpp_import.md)
- [トレイトの使い方](./doc/trait.md)
