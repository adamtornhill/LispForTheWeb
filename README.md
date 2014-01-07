# Lisp for the Web

This is the source code accompanying my book [Lisp for the Web](https://leanpub.com/lispweb), a Common Lisp web development tutorial. You can get the book at [Leanpub](https://leanpub.com/lispweb). I hope you like it and may the [parentheses](http://xkcd.com/297/) be with you.

## Organization

There are two versions of the source code:

1. *web_with_proto_backend.lisp* : this is the initial code, developed with a prototypic in-memory backend. In the tutorial we migrate the code to a persistent storage.
2. *web_with_persistent_backend.lisp* : the same code but backed by a persistent storage. In the tutorial I illustrate how to integrate [mongoDB](http://www.mongodb.org) in Common Lisp.
