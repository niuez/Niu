## ToDo

- `struct`
- `impl`
- `Trait`
- `C++のインポート`

## Progress

### literal

- `u64`: 数字 (+ `u64`)
- `i64`: 数字 + `i64`
- `bool`: `true`か`false` 

### variable

- 数字から始まっていない
- 予約語に含まれていない
- `a-z, A-Z, 0-9, _` で構成される

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
