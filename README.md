guile-zlib
==========

A simple zlib wrapper for GNU Guile.

Usage
-----
```
scheme@(guile-user)> (use-modules (zlib))
scheme@(guile-user)> (utf8->string (uncompress (compress (string->utf8 "hello, world!"))))
$1 = "hello, world!"
```

Install
-------
```
./autogen.sh
./configure
make
sudo make install
```

License
-------
GNU LGPL v3
