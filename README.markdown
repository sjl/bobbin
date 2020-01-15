Bobbin
======

```
       .─────────────.
   _.─'      ( )      `──.
 ,'    ( )         ( )    `.
;          -------          :
:  ( )    |.......|    ( )  ;
 ╲         -------         ╱
  `.   ( )         ( )   ,'
 ,' `──.     ( )     _.─' `.
;       `───────────'       :
:  ( )    |       |    ( )  ;
 ╲         \_____/         ╱
  `.   ( )         ( )   ,'
    `──.     ( )     _.─'
        `───────────'
```

Bobbin is a simple word-wrapping library for strings in Common Lisp.  It depends
only on `split-sequence`.

It aims to be simple, work nicely for the majority of cases, and degrade
gracefully for edge cases.  It is not particularly concerned with speed — if you
need very high-performance word wrapping Bobbin is not for you.

Bobbin can be installed with Quicklisp: `(ql:quickload :bobbin)`.

* **License:** MIT
* **Documentation:** <https://docs.stevelosh.com/bobbin/>
* **Mercurial:** <https://hg.stevelosh.com/bobbin/>
* **Git:** <http://github.com/sjl/bobbin/>
