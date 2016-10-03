;;\section{\textit{preprocessors.rkt}}
;;```
#lang racket/base

(require "utils.rkt")


(define (uncomment-line-start line pattern)
  (if (startswith? line pattern)
      (substring line (string-length pattern))
      line))


(define (uncomment-lines-start pattern)
  (lambda (lines)
    (map (lambda (l)
           (uncomment-line-start l pattern))
         lines)))


(define *PREPROCESSORS*
  `(("uncomment-double-semicolon" ,(uncomment-lines-start ";;"))
    ("none" ,(lambda (l) l))))


(define (string->preprocessor s)
  (let ((preprocessor (assoc s *PREPROCESSORS*)))
    (unless preprocessor
      (error "Unsupported preprocessor: " s))
    (cadr preprocessor)))


(provide string->preprocessor)
