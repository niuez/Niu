# Niuの基本的な構文

## プリミティブ型

プリミティブ型には, 以下の種類があります. 対応するリテラルの例も示します.

- `u64`(`std::uint_fast64_t`)
  - 数字のみ `123`, `123_456`, `1_2_3_4_5_6`
  - `u64`を後ろにつける `123u64`, `123_456u64`
- `i64`(`std::int_fast64_t`)
  - `i64`を後ろにつける `123i64`, `123_456i64`
- `bool`(`bool`)
  - `true`
  - `false`
- `void`(`void`)
  - リテラルなし. voidは変数に代入できず, 関数の引数の型に取ることもできません.

## 変数宣言

`let`構文で変数を宣言できます. 型を明示することもできます.

```
let a = 1i64;
let b = true;
let c: u64 = 2u64;
```

これらをトランスパイルすると, `let a = 1i64`は`std::int_fast64_t a = 1ll`というように変数の型が明示されるようになります.

## 演算

二項演算には, 以下の種類があります. 優先順序が遅い順に並んでいます.

- `Or`(`||`)
- `And`(`&&`)
- `Ord`(`<`, `<=`, `>=`, `>`, `==`, `!=`)
- `BitOr`(`|`)
- `BitXor`(`^`)
- `BitAnd`(`&`)
- `Shift`(`<<`, `>>`)
- `Add`(`+`), `Sub`(`-`)
- `Mul`(`*`), `Div`(`/`), `Rem`(`%`)


`Or`と`And`については, 演算する型が`bool`でないといけません.  `Ord`はbool型を返すようになっています.

今のところ演算に関する型チェックは, **左辺と右辺の型が同じであること**しか確認していません. なので, C++側にトランスパイルしたときに不具合が起こることがあります.

## 単項

上の演算より優先順序が早いものです.

- 変数
- リテラル
- かっこ
  - `()`で式を囲います.
  - `let a = (1 + 2) * 3;`
- ブロック
  - `{ ..; ..; .. }`のように使います. 最後の式が返す値になります.
  - 最後の式がない場合, `void`を返します.
  - `let a = { let b = 1; b * 2 };`

以下は後述します.

- (関数呼び出し)
- (構造体のメンバ変数)
- (トレイトの関数呼び出し)
- (構造体のメンバ関数)

## if

ifは値を返すことができます. 以下の例では, `a == 0`であれば`x = 0`, そうでなければ`x = b / a`となります.

```
let x = if a == 0 {
  0
}
else {
  b / a
}
```

### 注意

C++に変換すると
```cpp
std::uint_fast64_t x = [&](){ if(a == 0ull) {
 return 0ull;
}
else {
 return b/a;
}
```

このように, ラムダ式が出てきます. とりあえずこの方法で値を代入する機能を実現していますが, 対策を練る必要がありそうです.(#5)

## 関数

以下に例を示します. ジェネリクスにも対応しています.

```
fn func_name(arg1: i64, arg2: i64) -> i64 {
  let sum = arg1 + arg2;
  sum * arg1
}

fn void_func() -> void {}

fn generics_func<T>(t: T) -> T {
  t
}
```

## 構造体

以下に例を示します. ジェネリクスにも対応しています.(最後の点はあってもなくてもいいです)

```
struct Hoge {
  a: i64,
  b: u64,
}

struct Huga<S, T> {
  s: S,
  t: T
}
```

構造体は, 以下のようにインスタンス化できます.

```
let hoge = Hoge { a: 1i64, b: 2u64 };
let huga = Huga { s: hoge, t: true };
```

## 構造体のメンバ関数

以下に例を示します.

```
impl Hoge {
  fn new(a: i64, b: u64) -> Hoge {
    Hoge { a: a, b: b }
  }
  fn get_a(hoge: Hoge) -> i64 {
    hoge.a
  }
  fn b_sum(hoge: Hoge, c: u64) -> u64 {
    hoge.b + c
  }
}

impl<S, T> Huga<S, T> {
  fn new_huga(s: S, t: T) -> Huga<S, T> {
    Huga { s: s, t: t }
  }
  fn get_s(huga: Huga<S, T>) -> S {
    huga.s
  }
}

let new_hoge = Hoge::new(1i64, 2u64);
let a1 = hoge.get_a();
let a2 = Hoge::get_a(hoge);

let b1 = hoge.b_sum(1u64);
let b2 = Hoge::b_sum(hoge, 1u64);

let new_huga = Huga::new_huga(1i64, 2u64);
let s1 = huga.get_s();
let s2 = Huga::get_s(huga);
```

a1とa2, b1とb2, s1とs2は等価です. `hoge.func`で呼び出すと`func`の第一引数に`hoge`が入ります.

(new\_hugaとs2が動きません なんで #6)
