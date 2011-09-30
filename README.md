[xyzzy.ansify] に取り込まれました。今後はそっちでメンテされるのでこっちは放置になります。

  [xyzzy.ansify]: http://github.com/bowbow99/xyzzy.ansify

これは何
========
type specifier （型指定子）を便利に。

関数 `typep` への変更
---------------------
- `deftype` で定義した type specifier を使えるように
- 未定義の type specifier だったら警告できるように（設定で変更可能）

型チェック式を最適化
--------------------
調べる値の式と type specifier からそれなりに最適化した型チェックをする式を
作る関数を用意しました。主に type specifier を利用するマクロ（もうちょっと
具体的に言うと `typecase`）で使うことを考えて作りました。
詳しくは `optimize-typep` や `optimize-typep-1` のリファレンスを。

    (optimize-typep '(typep X 'string))
    => (stringp X)
    
    (optimize-typep '(typep X '(integer 0 100)))
    => (and (integerp X) (<= 0 X 100))
    
    (optimize-typep-1 'X '(or string symbol))
    => (or (stringp X) (symbolp X))
    
    (optimize-typep-1 'X `(or function
                              (and symbol (satisfies fboundp))))
    => (or (functionp X)
           (and (symbolp X) (fboundp X)))

大体2~5倍くらい速くなるようです。（コンパイル後）

型チェックをする関数
--------------------
引数が指定された型指定子を満たすか判別する関数を返すマクロを用意しました。

    (typepred string)
    => #<function: stringp>
    
    (typepred (or string symbol))
    ;; == (lambda (#1=#:object) (or (stringp #1#) (symbolp #1#)))
    => #<lexical-closure: (anonymous)>
    
    (remove-if-not (typepred (or function
                                 (and symbol (satisfies fboundp))))
      (list 'list 'symbol (lambda (x) x) (typepred fixnum)))
    => (list #<lexical-closure: (anonymous)> #<function: *fixnump>)

インストール
============

NetInstaller から
-----------------
[カフェイン中毒][1] からどうぞ

  [1]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l


使い方（開発者向け）
===================
`typep` への変更は（上書きしているので）読み込んだだけで有効になります。
`optimize-typep` は typespec+ パッケージから `export`　してあるので、必要に応じて
`use-package` するかパッケージプレフィックス付きで指定してください。

    ;; たぶん `optimize-typep` とかを使うマクロを定義するのだろう、ってことで
    ;; `eval-when` してます。
    (eval-when (:load-toplevel :compile-toplevel :execute)
      (require "typespec+"))
    
    (use-package :typespec+)

注意点、既知の問題など
======================
- 関数 `typep` を上書きしています

バグ報告、質問、要望などは [GitHub Issues][GH-Issues] か [@bowbow99][twitter] あたりへ
お願いします。

  [GH-Issues]: http://github.com/bowbow99/xyzzy.typespec-plus/issues
  [twitter]: http://twitter.com/bowbow99/
