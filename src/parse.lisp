(in-package :cl-user)
(defpackage :chomsky
  (:use
    :cl :chomsky.parser :chomsky.combinators)
  (:export 
    :parse
    :choice-of
    :sequence-of))

(in-package :chomsky)

(defun compose (f g)
  (lambda (x)
    (funcall g (funcall f x))))

(defun parse (parsers input)
  "Given a set of combinators and an input,
   apply the resulting parser to the input."
  (funcall (reduce #'compose parsers) (chomsky.parser:parser-return input)))

(parse (list (sequence-of '(#\h #\e))
             (choice-of '(#\l #\l #\o))) 
       '(#\h #\e #\l #\l #\o))


(funcall (compose (choice-of '(#\l #\l #\o)) (sequence-of '(#\h #\e))) (chomsky.parser:parser-return '(#\h #\e #\l #\l)))

(funcall (choice-of '(#\l #\l #\o))
         (funcall (sequence-of '(#\h #\e)) (chomsky.parser:parser-return '(#\h #\e #\l #\l #\o))))

(funcall (sequence-of '(#\l #\l #\o)) (chomsky.parser:make-parser-result '(#\e #\h) '(#\l #\l)))

(term my-word (sequence-of '(#\h #\e))
              (choice-of '(#\l #\l #\o)))

(parse my-word '(#\h #\e #\l #\l #\o))

(macroexpand (term hel (sequence-of '(#\h #\e))))

(defmacro term (name &rest rules) 
  `(defvar ,name ,rules))

(defun choice-of (tokens)
  (chomsky.combinators:alternative (chomsky.parser:gen-parsers #'first #'rest #'char= tokens)))

(defun sequence-of (tokens)
  (chomsky.combinators:applicative (chomsky.parser:gen-parsers #'first #'rest #'char= tokens)))
