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

;;; example output 
;;; (#S(:parsed, :remaining),#S(:parsed, :remaining),(remaining))

(defun apply-to-nth (functions items)
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

(defun alternative (parsers tokens)
  "Alternative combination of parsers."
  (labels ((interior (parsers* tokens* result) 
             (cond ((null parsers*) result)
                   ((null (parser-result-parsed (funcall (first parsers*) (first tokens*)))) (interior (rest parsers*) tokens* result))
                   (t (interior parsers* (list (parser-result-remaining (funcall (first parsers*) (first tokens*)))) (append (map 'list (first parsers*) tokens*) result))))))
    (reverse (interior parsers tokens nil))))

(defun parser-compose (parser parser*) 
  (lambda (input)
    (let ((first-result (funcall parser* input)) 
          (second-result (funcall parser (list (parser-result-remaining (funcall parser* input))))))
      (list first-result second-result))))

(funcall (applicative (gen-parsers #'char= '(#\l #\o))) '("hello"))
(funcall (parser-compose (applicative (gen-parsers #'char= '(#\l #\o))) (applicative (gen-parsers #'char= '(#\h #\e #\l)))) '("hello"))
(funcall (alternative #'char= '(#\h #\e #\l)) '("hello"))
(funcall (parser-join (sequential #'char= '(#\h #\e #\l)) (choice #'char= '(#\o #\l))))
