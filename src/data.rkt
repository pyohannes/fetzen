;;\section{\textit{data.rkt}}
;;```
#lang racket/base


(require "utils.rkt")


(struct mode (code))


(define (mode-docu-make)
  (mode #f))


(define (mode-code-make)
  (mode #t))


(define (mode-code? mode)
  (mode-code mode))


(define (mode-docu? mode)
  (not (mode-code? mode)))


(define (mode-reverse mode)
  (if (mode-docu? mode)
      (mode-code-make)
      (mode-docu-make)))



(struct line (text number))


(define (line->mode line oldmode)
  (mode-reverse oldmode))


(define (line-comment? l)
  (startswith? (line-text l) "###"))


(define (line-instruction? l)
  (startswith? (line-text l) "```"))


(struct chunk (mode [lines #:mutable] filename))


(define (chunk-append-line c l)
  (set-chunk-lines! c (append (chunk-lines c) (list l))))


(provide (all-defined-out))
