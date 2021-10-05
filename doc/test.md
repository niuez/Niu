# ユニットテスト

Niu言語のユニットテストでは, [library-checker-problems](https://github.com/yosupo06/library-checker-problems)で使われている問題を使うことができます. 

## library.toml

`library.toml`にテストをするための設定を書いておきます. 以下は一例です.

```toml
compiler="g++"
compile_options=["-std=c++17"]

[[testers]]
name="library-checker-problems"
repo="https://github.com/yosupo06/library-checker-problems"
generator="generate.py"
```

- `compiler`: コンパイラの指定
- `compile_options`: コンパイラに渡すオプション
- testers
  - `name`: テストに使う問題集の名前
  - `repo`: 問題があるリポジトリのurl
  - `generator`: 入出力を生成するために使うスクリプトの指定

## ユニットテストの書きかた

`aplusb`を例に書いてみます.

```
import "std/u64.niu"

fn aplusb(a: u64, b: u64) -> u64 {
  a + b
}

testfn(library-checker-problems:sample/aplusb) aplusb_test $${
  long long a, b;
  std::cin >> a >> b;
  std::cout << aplusb(a, b) << "\n";
}$$
```

`testfn([テストの名前(name)]:[問題のディレクトリ]) [テスト名] $${ [C++のコード] }$$`

## テストの実行

`niu test`

`transpile/`にトランスパイルしたコードが出力され, `.test/`にテストに必要なコードが出力されます.
