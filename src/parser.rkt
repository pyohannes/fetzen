\section{\textit{parser.rkt}}
```
#lang racket/base


(require racket/file
         racket/vector)


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


(define (startswith? line substr)
  (let ((len (string-length substr)))
    (and (>= (string-length line) len)
         (equal? (substring line 0 len) substr))))


(define (comment? line)
  (startswith? line "###"))


(define (instruction? line)
  (startswith? line "```"))


(define (file->chunks src)

  (define (lines->chunks lines c)
    (if (null? lines)
        (list c)
        (let ((line (car lines))
              (rest (cdr lines))) 
          (cond ((comment? line)
                 (lines->chunks rest c))
                ((instruction? line)
                 (cons c
                   (lines->chunks 
                     rest
                     (chunk (line->mode line (chunk-mode c))))))
                (else
                  (set-chunk-lines! c (append (chunk-lines c) (list line)))
                  (lines->chunks rest c))))))

  (lines->chunks (file->lines src) (chunk (mode-docu-make))))


(provide (struct-out mode) mode-code? mode-docu? 
         (struct-out chunk) file->chunks)
