;;```((mode "documentation") (post ("replace-variables")))
;;\section{\textit{``(file-basename)``}}
;;
;;This module contains utility functions that are used by other parts of the
;;program, but are not provided by racket.
;;
;;The \textit{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The function \textit{startswith?} returns \textit{\#t} if the string 
;;\textit{line} starts with the string \textit{substr} and \textit{\#f} 
;;otherwise:
;;```
(define (startswith? line substr)
   (equal? substr
           (substring line 0 (min (string-length line)
                                  (string-length substr)))))
;;```
;;All defined functions are exported:
;;```
(provide (all-defined-out))
