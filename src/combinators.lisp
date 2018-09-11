(in-package #:chomsky)

(defun make-parser-result* (pending-result) 
  "Helper function to return a finalize parser result."
  (make-parser-result :parsed pending-result
                      :remaining (parser-result-remaining (first pending-result))))

(defun pending-result (parser tokens previous-result)
  "Helper function to build up parsing results."
  (append (list (funcall parser tokens)) previous-result))

(defun applicative (parsers) 
  "Applicative combination of parsers.
   Represents a sequence of parsers."
  (lambda (tokens)
    (labels ((interior (parsers* tokens* result)
                 (if (null parsers*)
                     (make-parser-result* result)
                     (interior (rest parsers*) 
                               (funcall (first parsers*) tokens*) 
                               (pending-result (first parsers*) tokens* result)))))
      (interior parsers tokens (parser-result-parsed tokens))))) (defun alternative (parsers)
  "Alternative combination of parsers.
   Represents a choice between parsing options."
  (lambda (tokens) 
   (labels ((interior (parsers* tokens* result) 
             (cond ((null parsers*) (make-parser-result* result))
                   ((null (parser-result-parsed (funcall (first parsers*) tokens*))) (interior (rest parsers*) tokens* result))
                   (t (interior parsers* 
                                (funcall (first parsers*) tokens*) 
                                (pending-result (first parsers*) tokens* result))))))
    (interior parsers tokens (parser-result-parsed tokens)))))

