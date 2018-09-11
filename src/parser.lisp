(defpackage chomsky)

;TODO: Factor out list conversion to a separate function.

(defstruct (parser-result)
  "A parser parser-result. Parsed and remaining contents."
  parsed 
  remaining)

(defun partial (f &rest args)
  (lambda (&rest args1)
    (apply f (append args args1))))

(defgeneric consume (f parse-result)
    (:documentation "Consume an element of some structure for parsing.")
    (:method ((f function)(parse-result parser-result))
      (funcall f (parser-result-remaining parse-result))))
 
(defgeneric next (f parse-result)
  (:documentation "Access the next item of a parser.")
  (:method ((f function) (parse-result parser-result))
    (funcall f (parser-result-remaining parse-result))))

(defun gen-parser (f g predicate token) 
  "Generate a parser using a consumer function, progression function, predicate and token."
  (lambda (input)
    (cond ((null (consume f input)) (make-parser-result :parsed nil :remaining nil))
          ((funcall predicate (consume f input) token) (make-parser-result :parsed (consume f input) :remaining (next g input)))
          (t (make-parser-result :parsed nil :remaining (next g input))))))

(defun gen-parsers (f g predicate tokens)
  "Generate parsers using the same predicate for a list of tokens."
  (map 'list (partial #'gen-parser f g predicate) tokens))

(defun parser-return (constant)
  "Lift some input into a parser.
   This can be used to initialize an applicative sequence of parsers."
  (lambda (input)
    (make-parser-result :parsed constant :remaining input)))

(defun parser-fail ()
  "Enforce a parser failure."
  (lambda (input) 
    (make-parser-result :parsed nil :remaining input)))

;; not technically a 'join' on parsers, but on lists.
(defun parser-join (parser-result)
  (if (listp (parser-result-parsed parser-result))
    (make-parser-result :parsed (map 'list #'parser-result-parsed (parser-result-parsed parser-result))
                        :remaining (parser-result-remaining parser-result))
    (make-parser-result :parsed (parser-result-parsed parser-result)
                        :remaining (parser-result-remaining parser-result))))
