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

## API Reference

TBD

## Author

lambda_sakura(lambda.sakura@gmail.com)

## Copyright

Copyright (c) 2014 lambda_sakura(lambda.sakura@gmail.com)

## License

MIT
