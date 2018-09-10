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
          (t (make-result :parsed nil :remaining (rest in)))))))

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

(defun parser-join (f)
  (lambda (parser)
    (funcall f (result-parsed ))))

(defun sequential (p symbols) 
  "Return a parser for all p symbols provided."
  (map 'list ))

;; Note this implementation of applicative is currently specific
;; to the parsing combinator structure.
(defun applicative (fs xs) 
  (labels ((interior (gs ys r) 
             (if (null gs)
                 (append (list (result-remaining (first r))) r)
                 (interior (rest gs) (list (result-remaining (funcall (first gs) (first ys)))) (append (map 'list (first gs) ys) r))))) 
    (reverse (interior fs xs nil))))

(defun partial (f &rest args)
  (lambda (&rest args1)
    (apply f (append args args1))))

(defun sequential (p symbols)
  "Use an 'applicative interface' to combine parsers."
  (let ((parsers (map 'list (partial #'gen-parser p) symbols)))
    (lambda (input) 
      (applicative parsers input))))

(defun alternative (fs xs) 
  (labels ((interior (gs ys r) 
             (cond ((null gs) r)
                   ((null (result-parsed (funcall (first gs) (first ys)))) (interior (rest gs) ys r))
                   (t (interior gs (list (result-remaining (funcall (first gs) (first ys)))) (append (map 'list (first gs) ys) r))))))
    (reverse (interior fs xs nil))))

(defun choice (p symbols)
  "Prove a choice between parsing alternatives."
  (let ((parsers (map 'list (partial #'gen-parser p) symbols)))
    (lambda (x)
     (alternative parsers x))))

(funcall (sequential #'char= '(#\h #\e #\l)) '("hello"))
(funcall (applicative-combine parser-a parser-a (gen-parser #\l)) '("hhllo"))
(funcall (choice #'char= '(#\h #\e #\l)) '("hello"))

(defun stream-to-list () 
  "Convert a stream to a list."
  ())

(coerce "hello" 'list)
(with-input-from-string (s "hello") (coerce (read s) 'string))
