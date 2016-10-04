;;\section{\textit{postprocessors.rkt}}
;;```
#lang racket/base

(require racket/list
         "utils.rkt"
         "parser.rkt")


(define (code? chunk)
  (mode-code? (chunk-mode chunk)))


(define (code chunks)
  (filter code? chunks))


(define (code-keep-line-numbers chunks)
  (map
    (lambda (c)
      (struct-copy 
        chunk 
        c
        (lines (if (code? c)
                   (chunk-lines c)
                   (make-list (+ (length (chunk-lines c)) 2) "")))))
    chunks))


(define (docu-text chunks)

  (define (prepend lines prefix)
    (map
      (lambda (l)
        (string-append prefix l))
      lines))

  (map
    (lambda (c)
      (cond [(code? c)
             (struct-copy
               chunk
               c
               (lines (append (list "")
                              (prepend (chunk-lines c) "  ")
                              (list ""))))]
            [else c]))
    chunks))


(define (docu-latex chunks)
  (map
    (lambda (c)
      (cond [(code? c)
             (struct-copy
               chunk
               c
               (lines (append (list (string-append "\\begin{" "lstlisting}"))
                              (chunk-lines c)
                              (list (string-append "\\end{" "lstlisting}")))))]
            [else c]))
    chunks))


(define *POSTPROCESSORS*
  `(("code" ,code)
    ("code-keep-line-numbers" ,code-keep-line-numbers)
    ("docu-text" ,docu-text)
    ("docu-latex" ,docu-latex)))


(define (string->postprocessor s)
  (let ((postprocessor (assoc s *POSTPROCESSORS*)))
    (unless postprocessor
      (error "Unsupported postprocessor: " s))
    (cadr postprocessor)))


(provide string->postprocessor)
