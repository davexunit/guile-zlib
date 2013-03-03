guile-zlib
==========

A simple zlib wrapper for GNU Guile.

Install
-------
```
./autogen.sh
./configure
make
sudo make install
```

Usage
-----
```
scheme@(guile-user)> (uncompress (compress "Hello. This is a test."))
$2 = "Hello. This is a test."
```

License
-------
GNU LGPL v3
