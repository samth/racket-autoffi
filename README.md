Racket AutoFFI
==============

This is a package which allows to automatically connect to a shared or dynamic
library.

**Note:** this is an experimental library and the API may still change

## Example

Say we have the following program in plain C:

**myprog.c**
```c
int add(int a, int b) {
  return a + b;
}
```

We can compile this program on OS X as follows:

```bash
$ clang -o myprog.dylib -shared myprog.c
``` 

This gives us a binary:

```bash
$ hexdump myprog.dylib
0000000 cf fa ed fe 07 00 00 01 03 00 00 00 06 00 00 00
0000010 0c 00 00 00 78 02 00 00 85 00 10 00 00 00 00 00
0000020 19 00 00 00 e8 00 00 00 5f 5f 54 45 58 54 00 00
0000030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0000040 00 10 00 00 00 00 00 00 00 00 00 00 00 00 00 00
...
```

The binary does not contain enough information to link to it. We need to add the missing information with the following command from [this project](http://github.com/samvv/Transit):

```bash
$ transc -json myprog.c > myprog.json
```

Importing this binary in Racket now requires only two lines of code. No need to
define complex FFI structures!;

**main.rkt**
```racket;
#lang racket
(require autoffi)
(require/foreign "myprog")
(add 1 2)
```

If we run this program, we get:

```bash
$ racket main.rkt
3
```

