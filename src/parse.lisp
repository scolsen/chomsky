(in-package #:chomsky)

(defun compose (f g)
  (lambda (x)
    (funcall g (funcall f x))))

(defun parse (parsers input)
  "Given a set of combinators and an input,
   apply the resulting parser to the input."
  (funcall (reduce #'compose parsers) (funcall (parser-return nil) input)))

(parse (list (sequence-of '(#\h #\e))
             (choice-of '(#\l #\l #\o))) 
       '(#\h #\e #\l #\l #\o))

(defun term (name &rest rules) 
  (defvar name 
          rules))

(term my-word (sequence-of '(#\h #\e))
              (choice-of '(#\l #\l #\o)))

my-word 

(parse my-word '(#\h #\e #\l #\l #\o))
(defmacro term (name &rest rules) 
  `(defvar ,name (list (list ,@rules))))

(defun choice-of (tokens)
  (alternative (gen-parsers #'first #'rest #'char= tokens)))

(defun sequence-of (tokens)
  (applicative (gen-parsers #'first #'rest #'char= tokens)))
