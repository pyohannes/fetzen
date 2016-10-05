;;\section{\textit{postprocessors.rkt}}
;;```
#lang racket/base

(require racket/list
         "utils.rkt"
         "data.rkt")


(define (code? chunk)
  (mode-code? (chunk-mode chunk)))


(define (code chunks)
  (filter code? chunks))


(define (code-keep-line-numbers chunks)

  (define (add-missing-lines lines pos)
    (cond [(null? lines) '()]
          [(= (line-number (car lines)) pos) 
           (cons (car lines) (add-missing-lines (cdr lines) (+ pos 1)))]
          [else
           (cons (line "" -1) (add-missing-lines lines (+ pos 1)))]))

  (define (add-lines-in-chunks chunks pos)
    (cond [(null? chunks) '()]
          [else
           (let* ([c (car chunks)]
                  [new-lines (add-missing-lines (chunk-lines c) pos)])
             (cons (struct-copy chunk c (lines new-lines))
                   (add-lines-in-chunks (cdr chunks) 
                                        (+ pos (length new-lines)))))]))
 
  (add-lines-in-chunks (filter code? chunks) 1))


(define (docu-text chunks)

  (define (prepend lines prefix)
    (map
      (lambda (l)
        (struct-copy 
          line 
          l 
          (text (string-append prefix (line-text l)))))
      lines))

  (map
    (lambda (c)
      (cond [(code? c)
             (struct-copy
               chunk
               c
               (lines (append (list (line "" -1))
                              (prepend (chunk-lines c) "  ")
                              (list (line "" -1)))))]
            [else c]))
    chunks))


(define (docu-latex chunks)
  (map
    (lambda (c)
      (cond [(code? c)
             (struct-copy
               chunk
               c
               (lines (append (list (line (string-append "\\begin{" "lstlisting}") -1))
                              (chunk-lines c)
                              (list (line (string-append "\\end{" "lstlisting}") -1)))))]
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
