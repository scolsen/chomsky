(defpackage chomsky)

;TODO: Factor out list conversion to a separate function.

(defstruct (parser-result)
  "A parser parser-result. Parsed and remaining contents."
  parsed 
  remaining)

(defun partial (f &rest args)
  (lambda (&rest args1)
    (apply f (append args args1))))

(defun gen-parser (predicate token) 
  "Generate a parser using a predicate and token."
  (lambda (input)
    (let ((input* (coerce input 'list)))
    (cond ((funcall predicate (first input*) token) (make-parser-result :parsed (first input*) :remaining (rest input*)))
          (t (make-parser-result :parsed nil :remaining (rest input*)))))))

(defun gen-parsers (predicate tokens)
  "Generate parsers using the same predicate for a list of tokens."
  (map 'list (partial #'gen-parser predicate) tokens))

(defun parser-return (constant)
  "Lift some input into a parser.
   This can be used to initialize an applicative sequence of parsers."
  (lambda (input)
    (make-parser-result :parsed constant :remaining input)))

(defun parser-fail ()
  "Enforce a parser failure."
  (lambda (input) 
    (make-parser-result :parsed nil :remaining input)))

(defun apply-to-first (functions items)
  "Apply the first function in function to the first item."
  (funcall (first functions) (first items)))

(defun applicative (parsers) 
  "Applicative combination of parsers."
  (lambda (tokens)
    (labels ((interior (parsers* tokens* result)
                 (if (null parsers*)
                     (make-parser-result :parsed (reverse result) :remaining (parser-result-remaining (first result)))
                     (interior (rest parsers*) (list (parser-result-remaining (funcall (first parsers*) (first tokens*)))) (append (map 'list (first parsers*) tokens*) result))))) 
      (interior parsers tokens nil))))

(defun alternative (parsers)
  "Alternative combination of parsers."
  (lambda (tokens) 
   (labels ((interior (parsers* tokens* result) 
             (cond ((null parsers*) (make-parser-result :parsed (reverse result) :remaining (parser-result-remaining (first result))))
                   ((null (parser-result-parsed (funcall (first parsers*) (first tokens*)))) (interior (rest parsers*) tokens* result))
                   (t (interior parsers* (list (parser-result-remaining (funcall (first parsers*) (first tokens*)))) (append (map 'list (first parsers*) tokens*) result))))))
    (interior parsers tokens nil))))

(defun parser-compose (parser parser*) 
  (lambda (input)
    (let ((first-result (funcall parser* input)) 
          (second-result (funcall parser (list (parser-result-remaining (funcall parser* input))))))
      (parser-join (parser-result-parsed (make-parser-result :parsed (list first-result second-result) :remaining (parser-result-remaining second-result)))))))

(defun parser-join (parser-results)
  (make-parser-result :parsed (reduce #'append (map 'list #'(lambda (x) (map 'list #'parser-result-parsed (parser-result-parsed x))) parser-results)) 
                      :remaining (parser-result-remaining (first (last parser-results)))))

(funcall (applicative (gen-parsers #'char= '(#\l #\o))) '("hello"))
(funcall (parser-compose (applicative (gen-parsers #'char= '(#\l #\o))) (applicative (gen-parsers #'char= '(#\h #\e #\l)))) '("hello"))
(funcall (alternative (gen-parsers #'char= '(#\h #\e #\l))) '("hello"))
