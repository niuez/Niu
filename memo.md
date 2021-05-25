## ToDo

- `struct`
- `impl`
- `Trait`
- `C++のインポート`

## Trait

### 関数呼び出しについて

`x.func(...)`を, xが持っているトレイト中の関数の呼び出しに対応させる.  
わかりやすさのため, 最初は`x.MyTrait::func(...)`という形で実装する(自動解決はさせない).

### genericsのトレイト境界チェック

トレイト境界チェックはいつするの

例

```
fn func<T: MyTrait>(t: Vec<T>) -> T::Output {
  if t.is_empty() {
    T::MyTrait::default()
  }
  else {
    t[0].MyTrait::out()
  }
}

let t = Vec::new();
let o = func(t);
func2(o); // ここの型チェックを遅延させる必要があるかも
t.push(T{}); // ここで型がきまる
```

Rustで動作確認済み

```rust
use std::fmt::Display;

trait MyTrait {
    type Output;
    fn default() -> Self::Output;
    fn out(&self) -> Self::Output;
}


fn my_func<T: MyTrait>(t: &Vec<T>) -> T::Output {
  if t.is_empty() {
    T::default()
  }
  else {
    t[0].out()
  }
}

struct Test {}

impl MyTrait for Test {
    type Output = i64;
    fn default() -> Self::Output {
        0
    }
    fn out(&self) -> Self::Output {
        1
    }
}

fn func2<T: Display>(n: T) {
    println!("{}", n);
}

fn main() {
    let mut t = Vec::new();
    let o = my_func(&t);
    func2(o); // ここの型チェックを遅延させる必要があるかも
    t.push(Test{}); // ここで型がきまる
}
```

参考: https://rustc-dev-guide.rust-lang.org/traits/resolution.html

まずは
```
trait MyTrait {
  type Output;
}
```
だけの実装を目指す.

- `i64#Mytrait::Output`の実装
  - `fn i64_mytrait() -> i64#Mytrait::Output { }`をかけるようにする.
  - TypeIdの改修 -> TypeSpec
  - `solve_associated_type`によるAssociatedTypeの解決
  - `T#MyTrait::Output`は解決しようがない（解決の方法が違う）
    - Solvedのような扱いにして型変数で置き換える
- トレイト境界チェックの遅延を実装する.
  - `fn func<T: MyTrait>(t: T) -> T#Mytrait::Output {}`をかけるようにする.
  - T: MyTrait -> ImplTraitではなく、別の型を用意して, enumとする
    - Type::Type -> TypeSpecとして解決
- トレイトを含めたトランスパイル
- メンバ関数
  - traitとimplで定義に矛盾がないかのチェック
  

- TypeAnotationがぶっ壊れてきたので、改修が必要
  - アノテーションが必要な構文木に番号をたてて、それをあとで回収する

### Let Destruct

`let Hoge { a: a, b: Huga { c: c } } = hoge`は,

```
let tmp: Hoge = hoge;
let a = tmp.a;
let tmp1 = tmp.b;
let c = tmp1.c;
```

というように型変数を定義していく

### Generic Struct

```
struct Hoge<S, T> {
  s: S,
  t: T,
}
```

`Type`が`TypeId`だけでは足りなくなった.


## Progress

### literal

- `u64`: 数字 (+ `u64`)
- `i64`: 数字 + `i64`
- `bool`: `true`か`false` 

### variable

- 数字から始まっていない
- 予約語に含まれていない
- `a-z, A-Z, 0-9, \_` で構成される

### Expression

演算優先順序

- `Or`
- `And`
- `Ord`: 二項演算のみ(`a == b == c`はエラー)
- `BitOr`
- `BitXor`
- `BitAnd`
- `Shift`
- `Add`, `Sub`
- `Mul`, `Div`, `Rem`

### Subseq

- `Call`(関数呼び出し): `<unary_expr>(<args>)`

### Parentheses

`(<expression>)`

### Block

`<statement>; ... ; <expression>`

### LetDeclaration

`let <variable> = <expression>`

### Function

`fn <func-name>(<variable>: <type>) -> <type>`

### Generics

`fn <func-name><generics>(<variable>: <type>) -> <type>`

### TypeCheck


[
Equal(TypeVariable(Counter(3)), Type(TypeId(TypeId { id: Identifier { name: "i64" } }))),
HasTrait(TypeVariable(Counter(4)), TraitId { id: Identifier { name: "MyTrait" } }),
Equal(Func([TypeVariable(Counter(4))], AssociatedType(TypeVariable(Counter(4)), AssociatedType { trait_id: TraitId { id: Identifier { name: "MyTrait" } }, type_id: AssociatedTypeIdentifier { id: Identifier { name: "Associated" } } })), Func([TypeVariable(Counter(3))], TypeVariable(Counter(5)))),
Equal(TypeVariable(Counter(5)),AssociatedType(Type(TypeId(TypeId { id: Identifier { name: "i64" } })),AssociatedType { trait_id: TraitId { id: Identifier { name: "MyTrait" }}, type_id: AssociatedTypeIdentifier { id: Identifier { name: "Associated" } } }))]
