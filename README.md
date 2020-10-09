# arithimetic

四則演算を行う実行可能プログラム

## 実行方法

1. コマンドライン

```terminal
math "5 + 5"
> 5
```

2. ファイル指定

式が記述されているファイルを指定して実行

```expr.txt
5 + 5
10 + 15
```

```terminal
math --path expr.txt
> 10
  25
```

## モジュール構成

### Parser

文字列を解析するモジュール。

使用するライブラリ： [Parsec](https://hackage.haskell.org/package/parsec)

"5 + 5"という式があれば、それを`[Num 5, Plus, Num 5]`と解析できる

### Evaluate

解析された式を実行し、答えを算出するモジュール。

使用するライブラリ: [mtl](https://hackage.haskell.org/package/mtl)

解析された文字列を[操車場アルゴリズム](https://ja.wikipedia.org/wiki/%E6%93%8D%E8%BB%8A%E5%A0%B4%E3%82%A2%E3%83%AB%E3%82%B4%E3%83%AA%E3%82%BA%E3%83%A0)を用いて[逆ポーランド記法](https://ja.wikipedia.org/wiki/%E9%80%86%E3%83%9D%E3%83%BC%E3%83%A9%E3%83%B3%E3%83%89%E8%A8%98%E6%B3%95)に変換し、それを評価する。

例：

`[Num 10, Plus, Num 5]`を与えると、15を返す

### CLI


コマンドラインを定義するモジュール

使用するライブラリ: [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

実行方法は先述の通り、式を文字列として与える方法と、`--path`引数でファイルを指定する方法にする。


解析結果のみを返すのもありかもしれない。 "15 + 5"を与えると `[Num 15, Plus, Num 5]`を返すとか

`--parse`のような

`--o`または`--output`で出力先を指定可能


### その他

- 負の数はどうするか(14 + -14)