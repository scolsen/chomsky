(in-package :cl-user)
(defpackage :chomsky
  (:use
    :cl
    :chomsky.parser
    :chomsky.combinators)
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
  (funcall (reduce #'compose parsers) (funcall (chomsky.parser:parser-return nil) input)))

;;(parse (list (sequence-of '(#\h #\e))
;;             (choice-of '(#\l #\l #\o))) 
;;       '(#\h #\e #\l #\l #\o))

(defun term (name &rest rules) 
  (defvar name 
          rules))

;;(term my-word (sequence-of '(#\h #\e))
;;              (choice-of '(#\l #\l #\o)))
;;
;;(parse my-word '(#\h #\e #\l #\l #\o))

(defmacro term (name &rest rules) 
  `(defvar ,name (list (list ,@rules))))

(defun choice-of (tokens)
  (chomsky.combinators:alternative (chomsky.parser:gen-parsers #'first #'rest #'char= tokens)))

(defun sequence-of (tokens)
  (chomsky.combinators:applicative (chomsky.parser:gen-parsers #'first #'rest #'char= tokens)))
