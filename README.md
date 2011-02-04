これは何
========
type specifier （型指定子）を便利に。

関数 `typep` への変更
---------------------
- `deftype` で定義した type specifier を使えるように
- 未定義の type specifier だったら警告するように

型チェック式を最適化
--------------------
調べる値の式と type specifier からそれなりに最適化した型チェックをする式を
作る関数を用意しました。主に type specifier を利用するマクロ（もうちょっと
具体的に言うと `typecase`）で使うことを考えて作りました。
詳しくは `optimize-typep` や `optimize-typep-1` のリファレンスを。

    ;; `optimize-typep` がマクロ呼び出しの式を返すので `macroexpand` しています
    (macroexpand (optimize-typep '(typep X 'string)))
    => (stringp X)
    => t
    
    (macroexpand (optimize-typep '(typep X '(integer 0 100))))
    => (if (integerp X)
         (<= 0 X 100))
    => t
    
    (macroexpand (optimize-typep-1 'X `(or function
                                           (and symbol (satisfies fboundp)))))
    => (let (#1=#:tem)
         (if (setq #1# (typep X 'function))
           #1#
           ;; 手動で `macroexpand` してあります
           (if (typep X 'symbol)
             (fboundp X))))
    => t

大体2~5倍くらい速くなるようです。（コンパイル後）

type specifier から関数生成
---------------------------
type specifier から、受け取ったオブジェクトがその方かどうかを調べる関数を
生成します。

    (remove-if-not (typepred `(or function symbol)) ...)
    ;; ≡(remove-if-not (lambda (x) (typep x '(or function symbol))) ...)

インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒][1] からどうぞ</del>

  [1]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

しばしお待ちを。

使い方（開発者向け）
===================
`typep` への変更は（上書きしているので）読み込んだだけで有効になります。
`optimize-typep` や `typepred` は typespec+ パッケージから `export`　してある
ので、必要に応じて `use-package` するかパッケージプレフィックス付きで指定して
ください。

    ;; たぶん `optimize-typep` とかを使うマクロを定義するのだろう、ってことで
    ;; `eval-when` してます。
    (eval-when (:load-toplevel :compile-toplevel :execute)
      (require "typespec+"))
    
    (use-package :typespec+)

注意点、既知の問題など
======================
- 関数 `typep` を上書きしています
- （特にエンドユーザには）警告がやかましいかも

バグ報告、質問、要望などは [GitHub Issues][GH-Issues] か [@bowbow99][twitter] あたりへ
お願いします。

  [GH-Issues]: http://github.com/bowbow99/xyzzy.typespec-plus/issues
  [twitter]: http://twitter.com/bowbow99/

