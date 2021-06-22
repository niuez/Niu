# NiuのC++ import機能

Niuでは, Niuで定義した関数や構造体のトランスパイルの方法を, 自分で設定することもできます.

以下は`std::vector`をNiu側で`Vec`として使えるようにするためのコードです.

- `$ty()`: ジェネリクス引数
- `$arg()`: 関数の引数

```
struct Vec<T> $${std::vector<$ty(T)>}$$

impl<T> Vec<T> {
  fn new() -> Self $${std::vector<$ty(T)>()}$$
  fn push(self: Self, t: T) -> bool $${$arg(self).push_back($arg(t))}$$
}

fn main() -> Vec<i64> {
  let vec = Vec::new();
  vec.push(1i64);
  vec
}
```

トランスパイルすると以下のようになります.

```cpp
#include <bits/stdc++.h>

std::vector<std::int_fast64_t> main() {
std::vector<std::int_fast64_t> vec = std::vector<std::int_fast64_t>();
vec.push_back(1ll);
return vec;

}
```
