(defpackage chomsky)

;TODO: Factor out list conversion to a separate function.

(defstruct (result)
  "A parser result. Parsed and remaining contents."
  parsed 
  remaining)

(defun gen-parser (predicate sym) 
  "Make a parser."
  (lambda (input)
    (let ((in (coerce input 'list)))
    (cond ((funcall predicate (first in) sym) (make-result :parsed (first in) :remaining (rest in)))
          (t '())))))

(defun parser-fail ()
  "Enforce failure."
  (lambda (x) 
    '()))

(defun parser-return (sym)
  "Return a constant symbol."
  (lambda (input)
    (make-result :parsed sym :remaining input)))

(defvar parser-a (gen-parser #'char= #\h))

(funcall parser-a "hello")

;; Note this implementation of applicative is currently specific
;; to the parsing combinator structure.
(defun applicative (fs xs) 
  (labels ((interior (gs ys r) 
             (if (null gs)
                 (append (list (result-remaining (first r))) r)
                 (interior (rest gs) (list (result-remaining (funcall (first gs) (first ys)))) (append (map 'list (first gs) ys) r))))) 
    (reverse (interior fs xs nil))))

(defun sequential (&rest parsers)
  "Use an 'applicative interface' to combine parsers."
  (lambda (x) 
    (applicative parsers x)))

(defun alternative (fs xs) 
  (labels ((interior (gs ys r) 
             (if (null gs)
                 r
                 (interior (rest gs) ys (append (map 'list (first gs) ys) r))))) 
    (reverse (interior fs xs nil))))

(defun choice (&rest parsers)
  "Prove a choice between parsing alternatives."
  (lambda (x)
    (alternative parsers x)))

(funcall (applicative-combine parser-a parser-a (parser-return #\b)) '("hhllo"))
(funcall (choice-combine parser-a parser-a) '("hhllo"))

(defun stream-to-list () 
  "Convert a stream to a list."
  ())

(coerce "hello" 'list)
(with-input-from-string (s "hello") (coerce (read s) 'string))
