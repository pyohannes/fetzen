;;\section{\textit{parser.rkt}}
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


(define (line->mode line oldmode)
  (mode-reverse oldmode))


(struct chunk (mode [lines #:auto #:mutable])
  #:auto-value (list))


(define (comment? line)
  (startswith? line "###"))


(define (instruction? line)
  (startswith? line "```"))


(define (lines->chunks lines)

  (define (l->c lines c)
    (if (null? lines)
        (list c)
        (let ((line (car lines))
              (rest (cdr lines))) 
          (cond ((comment? line)
                 (l->c rest c))
                ((instruction? line)
                 (cons c
                   (l->c
                     rest
                     (chunk (line->mode line (chunk-mode c))))))
                (else
                  (set-chunk-lines! c (append (chunk-lines c) (list line)))
                  (l->c rest c))))))

  (l->c lines (chunk (mode-docu-make))))


(provide (struct-out mode) mode-code? mode-docu? 
         (struct-out chunk) lines->chunks)
