#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :bobbin)
(time (asdf:test-system :bobbin))
(quit)
