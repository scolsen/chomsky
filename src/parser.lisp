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
    (if (funcall predicate (consume f input) token) 
        (make-parser-result :parsed (consume f input) :remaining (next g input))
        (make-parser-result :parsed nil :remaining (next g input)))))

(defun gen-parser (predicate token) 
  "Generate a parser using a predicate and token.
   Monadic over lists."
  (lambda (input)
    (cond ((funcall predicate (first (parser-result-remaining input)) token) (make-parser-result :parsed (first (parser-result-remaining input)) :remaining (rest (parser-result-remaining input))))
          (t (make-parser-result :parsed nil :remaining (rest (parser-result-remaining input)))))))

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

(defun compose (f g)
  "Return the composition of two functions.
   f after g."
  (lambda (&rest args)
    (funcall f (apply g args))))

(defun make-parser-result* (pending-result) 
  (make-parser-result :parsed (reverse pending-result)
                      :remaining (parser-result-remaining (first pending-result))))

(defun pending-result (parser tokens previous-result)
  (append (list (funcall parser tokens)) previous-result))

(defun next-tokens (parser-result)
  "Generate the list of remaining tokens for a next applicative pass."
  (list (parser-result-remaining (funcall (first parsers) (first tokens)))))

(defun applicative (parsers) 
  "Applicative combination of parsers.
   Represents a sequence of tokens."
  (lambda (tokens)
    (labels ((interior (parsers* tokens* result)
                 (if (null parsers*)
                     (make-parser-result* result)
                     (interior (rest parsers*) 
                               (funcall (first parsers*) tokens*) 
                               (pending-result (first parsers*) tokens* result)))))
      (interior parsers tokens nil))))

(defun alternative (parsers)
  "Alternative combination of parsers.
   Represents a choice between tokens."
  (lambda (tokens) 
   (labels ((interior (parsers* tokens* result) 
             (cond ((null parsers*) (make-parser-result* result))
                   ((null (parser-result-parsed (funcall (first parsers*) (first tokens*)))) (interior (rest parsers*) tokens* result))
                   (t (interior parsers* 
                                (next-tokens parsers* tokens*) 
                                (pending-result (first parsers*) tokens* result))))))
    (interior parsers tokens nil))))

(defun parser-compose* (&rest parsers)
  (lambda (input) 
   (reduce #'parser-join (rest parsers) :initial-value input)))

(defun parser-compose (parser parser*) 
  (lambda (input)
    (let ((first-result (funcall parser* input)) 
          (second-result (funcall parser (list (parser-result-remaining (funcall parser* input))))))
      (parser-join (parser-result-parsed (make-parser-result :parsed (list first-result second-result) :remaining (parser-result-remaining second-result)))))))

(defun parser-join (&rest parser-results)
  (make-parser-result :parsed (append (map 'list #'(lambda (x) (map 'list #'parser-result-parsed (parser-result-parsed x))) parser-results))
                      :remaining (parser-result-remaining (first (last parser-results)))))

(defun parser-join (parser-results r)
  (make-parser-result :parsed (reduce #'append (map 'list #'(lambda (x) (map 'list #'parser-result-parsed (parser-result-parsed x))) parser-results)) 
                      :remaining (parser-result-remaining (first (last parser-results)))))

(funcall (applicative (gen-parsers #'first #'rest #'char= '(#\l #\o))) (funcall (parser-return nil) '(#\h #\e #\l #\l #\o)))
(funcall (parser-compose (applicative (gen-parsers #'char= '(#\l #\o))) (applicative (gen-parsers #'char= '(#\h #\e #\l)))) '("hello"))
(funcall (parser-compose* (applicative (gen-parsers #'char= '(#\l #\o))) (applicative (gen-parsers #'char= '(#\h #\e #\l)))) (list (funcall (parser-return nil) "hello")))
(funcall (alternative (gen-parsers #'char= '(#\h #\e #\l))) '("hello"))
