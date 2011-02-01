これは何
========
`typep` などで使用できる type specifier （型指定子）を強化します。

予定
----
1. `deftype` で定義した type specifier を `typep` で使えるように
2. `typep` に未定義の type specifier を渡した場合に警告する
3. `typep` 式を最適化する関数 `optimize-typep`
4. type specifier から、受け取ったオブジェクトがその型かどうか調べる関数を作る

