# Traitの定義

例を示します.

```
trait Sum {
  type Output;
  fn sum(self: Self) -> Self#Sum::Output;
}
```

`Self`はトレイトが実装される型を指します.  
typeで宣言したものを関連型といいます. 関連型は`型#トレイト::関連型`で使うことができます.

以下は無効です. 関連型が必ず先に来るように定義してください.

```
trait Sum {
  fn sum(self: Self) -> Self#Sum::Output;
  type Output;
}
```

# impl

試しに`i64`を二つ持つ構造体`Pair`にSumを実装してみます.

```
struct Pair {
  a: i64,
  b: i64,
}

impl Sum for Pair {
  type Output = i64;
  fn sum(self: Pair) -> Pair#Sum::Output {
    self.a + self.b
  }
}
```

呼び出し方は以下の通りです.

```
let p = Pair { a: 9i64, b: 1i64, };
p.sum();
Pair#Sum::sum(p);
```

メンバ関数のように呼び出す方法と, `型#トレイト::関数名`で呼び出す方法の2種類があります.

# where節

where節を使うと, ジェネリクス型にトレイト制約をつけることができます.

## 関数にwhere節をつける例

```
fn apply_sum<T>(t: T) -> T#Sum::Output where T: Sum {
  t.sum()
}
```

関連型の型を指定することもできます. 

```
fn apply_sum<T>(t: T) -> T#Sum::Output#Sum::Output
where
  T: Sum,
  T#Sum::Output: Sum
{
  t.sum().sum()
}
```

ただし, whereの順番に気をつけてください. `where T#Sum::Output: Sum, T: Sum`とすると`T#Sum::Output`を解決できずにコンパイルエラーを起こします.

## implにwhere節をつける例

先程の`Pair`をジェネリクス化した`Pair<S, T>`にSumを実装することを考えます.  
`s`の`sum`と, `t`の`sum`が足し算できるかは`Add`トレイトを使うことで解決すると, 以下のようにかけます.

```
struct Pair<S, T> {
  s: S,
  t: T,
}

trait Add {
  type Right;
  type Output;
  fn add(a: Self, r: Self#Add::Right) -> Self#Add::Output;
}

trait Sum {
  type Output;
  fn sum(a: Self) -> Self#Sum::Output;
}

impl<S, T> Sum for Pair<S, T>
where
  S: Sum,
  T: Sum,
  S#Sum::Output: Add(Right=T#Sum::Output)
{
  type Output = S#Sum::Output#Add::Output;
  fn sum(a: Self) -> Self#Sum::Output {
    a.s.sum().add(a.t.sum())
  }
}
```
