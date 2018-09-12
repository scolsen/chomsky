(in-package :cl-user)
(defpackage :chomsky.parser
  (:use
    :cl)
  (:export
    :parser-result
    :make-parser-result
    :parser-result-remaining
    :parser-result-parsed
    :parser-return
    :parser-bind
    :parser-fail
    :parser-join
    :gen-parser
    :gen-parsers))

(in-package :chomsky.parser)

(defstruct (parser-result (:constructor make-parser-result (parsed remaining)))
  "A parser parser-result. Parsed and remaining contents."
  parsed 
  remaining
  )

(defun lift (h f g)
  (lambda (x)
    (funcall h (funcall f x) (funcall g x))))

(defun partial (f &rest args)
  (lambda (&rest args1)
    (apply f (append args args1))))

(defun compose (f g) 
  (lambda(x)
      (funcall f (funcall g x))))

(defun apply-to-remaining (f) 
  (lambda (parser-result)
    (funcall (compose f #'parser-result-remaining) parser-result)))

(defun parser-return (x)
  (funcall (lift #'make-parser-result (constantly nil) (constantly x)) nil))

(defun parser-bind (f g)
  (lift #'make-parser-result (apply-to-remaining f) (apply-to-remaining g)))

(defun parser-fail ()
  (funcall (lift #'make-parser-result (constantly nil) (constantly nil)) nil))

(defun parser-join (parser-result)
  (if (listp (parser-result-parsed parser-result))
    (make-parser-result (map 'list #'parser-result-parsed (parser-result-parsed parser-result))
                        (parser-result-remaining parser-result))
    (make-parser-result (parser-result-parsed parser-result)
                        (parser-result-remaining parser-result))))

(defun gen-parser (f g predicate token)
  (lambda (parser-result)
    (let ((bound (parser-bind f g)))
      (cond ((null (parser-result-remaining parser-result)) parser-result)
            ((funcall predicate token (funcall f (parser-result-remaining parser-result))) (funcall bound parser-result))
            (t (parser-fail))))))

(defun gen-parsers (f g predicate tokens)
  (map 'list (partial #'gen-parser f g predicate) tokens))
