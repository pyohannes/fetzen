;;```((mode "documentation") (post ("replace-variables")))
;;\section{\tc{``(file-basename)``}}
;;
;;This module contains utility functions that are used by other parts of the
;;program, but are not provided by racket.
;;
;;The \tc{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The function \tc{startswith?} returns \tc{\#t} if the string 
;;\tc{line} starts with the string \tc{substr} and \tc{\#f} 
;;otherwise:
;;```
(define (startswith? line substr)
   (equal? substr
           (substring line 0 (min (string-length line)
                                  (string-length substr)))))
;;```
;;\tc{hash*} is equivalent to the racket function \tc{hash}, however it
;;does accept all arguments in one list. This shortcut provides for more
;;readable code.
;;```
(define (hash* lst)
  (apply hash lst))
;;```
;;All defined functions are exported:
;;```
(provide (all-defined-out))
