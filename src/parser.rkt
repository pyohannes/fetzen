;;\section{\textit{parser.rkt}}
;;```
#lang racket/base


(require racket/file
         racket/list
         "data.rkt"
         "preprocessors.rkt")


(define (file->chunks filename pp)

  (define (lines->chunks lines c)
    (if (null? lines)
        (list c)
        (let ((l (car lines))
              (rest (cdr lines))) 
          (cond ((line-comment? l)
                 (lines->chunks rest c))
                ((line-instruction? l)
                 (cons c
                   (lines->chunks
                     rest
                     (chunk (line->mode l (chunk-mode c)) '() filename))))
                (else
                  (chunk-append-line c l)
                  (lines->chunks rest c))))))

  (let ([composed-preprocessor (apply compose1 (map string->preprocessor pp))])
    (lines->chunks 
      (composed-preprocessor (file->enumerated-lines filename))
      (chunk (mode-docu-make) '() filename))))


(define (file->enumerated-lines filename)
  (let ([lines (file->lines filename)])
    (map
      line
      lines (range 1 (+ (length lines) 1)))))


(provide file->chunks)
