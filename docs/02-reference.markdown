# API Reference

The following is a list of all user-facing parts of Bobbin.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `BOBBIN`

### `WRAP` (function)

    (WRAP STRING-OR-STRINGS WIDTH)

Wrap `string-or-strings` to `width`.

  `string-or-strings` can be a string or a list of strings.  A list of strings
  is treated as multiple lines.  In either case the string(s) may also contain
  newlines.  All of these linebreaks will be included in the output — wrapping
  will only add linebreaks, never remove them.

  The result with be of the same type as the argument: either a single string
  (containing newlines) or a list of strings (not containing newlines).

  Examples:

    (print (wrap (format nil "foo bar baz") 3))
    foo
    bar
    baz

    (print (wrap (format nil "foo bar baz") 7))
    foo bar
    baz

    (print (wrap (format nil "foo~%bar baz") 7))
    foo
    bar baz

    (print (wrap '("foo" "bar baz") 7))
    ("foo" "bar baz")

    (print (wrap '("foo" "bar baz") 3))
    ("foo" "bar" "baz")

  

