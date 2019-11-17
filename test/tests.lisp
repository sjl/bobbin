(in-package :bobbin.test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(intern (concatenate 'string (symbol-name 'test-) (symbol-name name)))
    (let ((*package* ,*package*))
      ,@body)))


(defun run-tests ()
  (1am:run))

(defun f (&rest args)
  (apply #'format nil args))

(defmacro check (input width result)
  (if (stringp input)
    `(is (string= (format nil ,result)
                  (bobbin:wrap (f ,input) ,width)))
    `(is (equal ',result (bobbin:wrap (mapcar #'f ',input) ,width)))))


;;;; Tests --------------------------------------------------------------------
(define-test noop
  (check "" 10 "")
  (check "foo bar" 10 "foo bar"))

(define-test basic-strings
  (check "foo bar baz" 11 "foo bar baz")
  (check "foo bar baz" 10 "foo bar~%baz")
  (check "foo bar baz" 3 "foo~%bar~%baz")
  (check "foo bar baz" 5 "foo~%bar~%baz")
  (check "foo bar baz" 6 "foo~%bar~%baz"))

(define-test long-words
  (check "abcdefghijklmnopqrstuvwxyz" 5 "abcde~%fghij~%klmno~%pqrst~%uvwxy~%z")
  (check "foo abcdefghijklmnopqrstuvwxyz" 5 "foo~%abcde~%fghij~%klmno~%pqrst~%uvwxy~%z"))

(define-test spaces
  (check "foo  bar  baz" 100 "foo  bar  baz")
  (check "foo  bar  baz" 4 "foo~%bar~%baz")
  (check "foo  bar  baz" 8 "foo  bar~%baz")
  (check "foo  bar  baz" 9 "foo  bar~%baz")
  (check "foo  bar  baz" 10 "foo  bar~%baz")
  (check "foo  bar  baz" 11 "foo  bar~%baz"))

(define-test markdown
  (check "This is a paragraph of text.  It contains some words and some spaces."
         20
         "This is a paragraph~@
          of text.  It~@
          contains some words~@
          and some spaces.")
  (check "Here is a list of a couple of things:~@
          ~@
          * foo~@
          * bar"
         20
         "Here is a list of a~@
          couple of things:~@
          ~@
          * foo~@
          * bar"))

(define-test indentation
  (check "   foo~% bar" 50 "   foo~% bar")
  (check "            foo          bar" 3 "foo~%bar")
  (check "  foo~%  bar" 5 "  foo~%  bar")
  (check ("  foo~%  bar") 5 ("  foo" "  bar")))

(define-test lists
  (check ("foo bar baz")
         3
         ("foo" "bar" "baz"))
  (check ("foo bar baz")
         7
         ("foo bar" "baz"))
  (check ("foo" "bar baz")
         7
         ("foo" "bar baz")))
