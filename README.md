guile-zlib
==========

A simple zlib wrapper for GNU Guile.

Usage
-----
```
scheme@(guile-user)> (uncompress (compress "Hello. This is a test."))
$2 = "Hello. This is a test."
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
