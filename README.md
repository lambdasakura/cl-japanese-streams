# Japanese stream for Common Lisp

## Usage

    (with-open-japanese-file ((stream "japanese-text.txt"))
	    (read-char stream)
		(read-line stream)
		...
	)

    (setq stream (make-japanese-input-stream "日本語"))
	(read-char stream)


## Description

cl-japanese-stream allows you to manipulate Japanese string as stream.

cl-japanese-streamはCommon Lisp処理系で日本語データの読み書きをStreamとして
扱うための関数を提供します。

Common Lisp で日本語のファイルを扱う場合、usb8 な stream として扱う例をよく目にします。
この方法で多くの場合は上手く扱えます。
しかし、この stream に対して read-char, read-line などを実行しても得られるものはあくまで usb8 な vector
として read-char した結果となります。
このためあるファイルから3文字読み取る。と言ったことが難しくなってしまいます。

cl-japanese-stream はこの問題をある程度解決するものです。
ファイルの入力を usb8-vector として読み込んだ後、babel を利用し string に変換したものを stream として提供します。
こうすることで、Lisp処理系からはstringのstreamデータとして扱うことが可能となり
read-char, read-line を問題なく実行することを可能にします。

## API Reference

with-open-japanese-file

指定したファイルをjapanese-input-streamとして開く。
このstreamに対して、read-charやread-lineを実行することで日本語データを読み取ることができる。

make-japanese-input-stream

Lisp処理系のstringをread-char, read-line などができるstreamに変換します。


## Author

lambda_sakura(lambda.sakura@gmail.com)

## Copyright

Copyright (c) 2014 lambda_sakura(lambda.sakura@gmail.com)

## License

MIT
