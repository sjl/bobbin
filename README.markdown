Bobbin is a simple (50 LOC) word-wrapping library for strings in Common Lisp.
It depends only on `split-sequence`.

It aims to be simple, work nicely for the majority of cases, and degrade
gracefully for edge cases.  It is not particularly concerned with speed — if you
need very high-performance word wrapping Bobbin is not for you.

* **License:** MIT
* **Documentation:** <http://github.com/sjl/bobbin/> (this `README`)
* **Mercurial:** <http://bitbucket.org/sjl/bobbin/>
* **Git:** <http://github.com/sjl/bobbin/>

Documentation
-------------

Bobbin's API only has a single function:

    (bobbin:wrap string-or-strings width)

The simplest way to use Bobbin is to pass it a string:

    (bobbin:wrap "hello, world!" 10)
    "hello,
    world!"

Every line in the string returned by `wrap` will contain at most `width`
characters (not including the newline itself).

Philosophy
----------

Bobbin will try to break lines at whitespace.  It will only break a word in the
middle if there's no other choice.  It does not try to hyphenate, or parse
hyphenation:

    (bobbin:wrap "This is a test of Bobbin's line-breaking." 10)
    "This is a
    test of
    Bobbin's
    line-break
    ing."

Initial whitespace (e.g. indentation) will be preserved, unless even the first
word cannot be fit if it were included.  Bobbin does not try to indent any
broken lines, but this may be added in the future:

    (bobbin:wrap "    foo bar baz" 10)
    "    foo
    bar baz"

    (bobbin:wrap "    thisisjusttoolong" 10)
    "thisisjust
    toolong"

Whitespace between words will be preserved, unless a line is broken at that
point.  This does the right thing for those of us who put two spaces after
a period, [as God
intended](https://web.archive.org/web/20171125050610/http://www.heracliteanriver.com/?p=324):

    (bobbin:wrap "there  are  two  spaces  between  these  words" 12)
    "there  are
    two  spaces
    between
    these  words"

Existing line breaks in the text are preserved.  Bobbin will only ever *add*
line breaks, never remove them:

    (bobbin:wrap (format nil "foo~%bar baz frob") 7)
    "foo
    bar baz
    frob"

Lists
-----

For convenience, you can also pass `wrap` a list of strings.  Each string is
treated as a separate line and wrapped as described above.  The results are
returned as a (flat) list of lines, each of which will be no more than `width`
characters long:

    (bobbin:wrap '("here is a line." "" "and here is another line") 8)
    ("here is"
     "a line."
     ""
     "and here"
     "is"
     "another"
     "line")

TODO
----

* Handle tab characters.
* Handle non-printing characters.
* Handle wide characters.
* `unwrap` function to make writing paragraphs easier.
* Maybe reindent broken lines?
