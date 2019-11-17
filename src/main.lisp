(in-package :bobbin)

(defun wrap-line (line width)
  "Wrap the single-line string `line` to `width`, returning a multi-line string."
  (with-output-to-string (*standard-output*)
    (let ((pos 0)
          (spaces 0)
          (words (split-sequence:split-sequence #\space line))
          (fresh t)
          word
          len)
      (flet ((add (s)
               (incf pos (length s))
               (princ s))
             (linebreak ()
               (setf pos 0 spaces 0)
               (terpri)))
        (loop
          :until (null words)
          :do
          (setf word (pop words)
                len (length word))
          (cond
            ;; chomp leading whitespace
            ((and (not fresh) (zerop pos) (zerop len))
             nil)
            ;; if we have multiple spaces in a row, preserve them (maybe)
            ((zerop len)
             (incf spaces))
            ;; if we're dealing with a single word that's too long, reluctantly
            ;; split it into pieces
            ((and (zerop pos) (> len width))
             (setf fresh nil)
             (add (subseq word 0 width))
             (linebreak)
             (push (subseq word width) words))
            ;; if this would send us beyond the limit, break
            ((> (+ spaces len pos) width)
             (if fresh
               (setf pos 0 spaces 0)
               (linebreak))
             (setf fresh nil)
             (push word words))
            ;; otherwise concat
            (t
             (setf fresh nil)
             (add (make-string spaces :initial-element #\space))
             (add word)
             (setf spaces 1))))))))

(defun wrap-lines (strings width)
  "Wrap a list of `strings` to `width`, returning a list of strings."
  ;; This is mildly tricky because we want to correctly handle indented lines
  ;; inside of multiline strings inside the list.
  (let ((lines (mapcan
                 ;; Split and flatten any multiline strings in the list first.
                 (lambda (string)
                   (split-sequence:split-sequence #\newline string))
                 strings)))
    (mapcan
      ;; Then wrap each string in the list and flatten the results.
      (lambda (line)
        (split-sequence:split-sequence #\newline (wrap-line line width)))
      lines)))

(defun wrap-string (string width)
  "Wrap a multi-line string, returning a multi-line string."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (wrap-line line width))
                  (split-sequence:split-sequence #\newline string))))

(defun wrap (string-or-strings width)
  "Wrap `string-or-strings` to `width`.

  `string-or-strings` can be a string or a list of strings.  A list of strings
  is treated as multiple lines.  In either case the string(s) may also contain
  newlines.  All of these linebreaks will be included in the output — wrapping
  will only add linebreaks, never remove them.

  The result with be of the same type as the argument: either a single string
  (containing newlines) or a list of strings (not containing newlines).

  Examples:

    (print (wrap (format nil \"foo bar baz\") 3))
    foo
    bar
    baz

    (print (wrap (format nil \"foo bar baz\") 7))
    foo bar
    baz

    (print (wrap (format nil \"foo~%bar baz\") 7))
    foo
    bar baz

    (print (wrap '(\"foo\" \"bar baz\") 7))
    (\"foo\" \"bar baz\")

    (print (wrap '(\"foo\" \"bar baz\") 3))
    (\"foo\" \"bar\" \"baz\")

  "
  (check-type width (integer 1))
  (etypecase string-or-strings
    (string (wrap-string string-or-strings width))
    (list (wrap-lines string-or-strings width))))
