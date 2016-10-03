;;\section{\textit{utils.rkt}}
;;```
#lang racket/base

(define (startswith? line substr)
  (let ((len (string-length substr)))
    (and (>= (string-length line) len)
         (equal? (substring line 0 len) substr))))


(provide startswith?)
