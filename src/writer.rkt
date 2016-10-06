;;\section{\textit{handlers.rkt}}
;;```
#lang racket/base


(require "data.rkt"
         "postprocessors.rkt")


(struct writer (file postprocessors))


(define (make-writer filename pp-names)
  (writer (open-output-file filename #:exists 'replace)
          (map 
            string->postprocessor 
            (append pp-names *default-postprocessors*))))


(define (writer-write-lines w lines)
  (map (lambda (l)
         (fprintf (writer-file w) "~a~n" (line-text l)))
       lines))


(define (writer-postprocess w chunks)
  ((apply compose1 (writer-postprocessors w)) chunks))


(define (writer-write w chunks)
  (for-each 
    (lambda (chunk) 
      (writer-write-lines w (chunk-lines chunk)))
    (writer-postprocess w chunks)))


(define (process-chunks writers chunks)
  (for-each
    (lambda (writer)
      (writer-write writer chunks))
    writers))


(provide process-chunks make-writer)
