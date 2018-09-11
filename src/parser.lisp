(defpackage chomsky)

;TODO: Factor out list conversion to a separate function.

(defstruct (parser-result)
  "A parser parser-result. Parsed and remaining contents."
  parsed 
  remaining)

(defun gen-parser (predicate sym) 
  "Make a parser."
  (lambda (input)
    (let ((in (coerce input 'list)))
    (cond ((funcall predicate (first in) sym) (make-parser-result :parsed (first in) :remaining (rest in)))
          (t (make-parser-result :parsed nil :remaining (rest in)))))))

(defun parser-fail ()
  "Enforce failure."
  (lambda (x) 
    '()))

(defun parser-return (sym)
  "Return a constant symbol."
  (lambda (input)
    (make-parser-result :parsed sym :remaining input)))

(defun parser-join (&rest parsers)
  (lambda (input)
    ()))

;; Note this implementation of applicative is currently specific
;; to the parsing combinator structure.
(defun applicative (parsers tokens) 
  (labels ((interior (parsers* tokens* result) 
             (if (null parsers*)
                 (append (list (parser-result-remaining (first result))) result)
                 (interior (rest parsers*) (list (parser-result-remaining (funcall (first parsers*) (first tokens*)))) (append (map 'list (first parsers*) tokens*) result))))) 
    (reverse (interior parsers tokens nil))))

(defun partial (f &rest args)
  (lambda (&rest args1)
    (apply f (append args args1))))

(defun sequential (predicate tokens)
  "Use an 'applicative interface' to combine parsers."
  (let ((parsers (map 'list (partial #'gen-parser predicate) tokens)))
    (lambda (input) 
      (applicative parsers input))))

(defun alternative (parsers tokens) 
  (labels ((interior (parsers* tokens* result) 
             (cond ((null parsers*) result)
                   ((null (parser-result-parsed (funcall (first parsers*) (first tokens*)))) (interior (rest parsers*) tokens* result))
                   (t (interior parsers* (list (parser-result-remaining (funcall (first parsers*) (first tokens*)))) (append (map 'list (first parsers*) tokens*) result))))))
    (reverse (interior parsers tokens nil))))

(defun choice (predicate tokens)
  "Prove a choice between parsing alternatives."
  (let ((parsers (map 'list (partial #'gen-parser predicate) tokens)))
    (lambda (input)
     (alternative parsers input))))

(funcall (sequential #'char= '(#\h #\e #\l)) '("hello"))
(funcall (choice #'char= '(#\h #\e #\l)) '("hello"))
(funcall (parser-join (sequential #'char= '(#\h #\e #\l)) (choice #'char= '(#\o #\l))))
