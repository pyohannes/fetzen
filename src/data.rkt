;;\section{\textit{data.rkt}}
;;```
#lang racket/base


(require "utils.rkt")


(struct mode (code postprocessor-names))


(define (mode-docu-make)
  (mode 'docu '()))


(define (mode-code-make)
  (mode 'code '()))


(define (mode-code? mode)
  (eq? (mode-code mode) 'code))


(define (mode-docu? mode)
  (eq? (mode-code mode) 'docu))


(define (mode-code-reverse code)
  (if (eq? code 'code)
      'docu
      'code))


(struct line (text number))


(define (line->mode l oldmode)

  (define (read-instruction text)
    (let ([instr (read (open-input-string (substring text 3)))])
      (if (list? instr)
          instr
          '())))

  (define (get-instruction instr key default)
    (let ([i (assoc key instr)])
      (if i
          (cadr i)
          default)))

  (let ([instr (read-instruction (line-text l))])
    (mode (get-instruction instr 'mode (mode-code-reverse (mode-code oldmode)))
          (get-instruction instr 'post '()))))


(define (line-comment? l)
  (startswith? (line-text l) "###"))


(define (line-instruction? l)
  (startswith? (line-text l) "```"))


(struct chunk (mode [lines #:mutable] filename))


(define (chunk-append-line c l)
  (set-chunk-lines! c (append (chunk-lines c) (list l))))


(provide (all-defined-out))
