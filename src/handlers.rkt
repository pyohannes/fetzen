\section{\textit{handlers.rkt}}
```
#lang racket/base


(require racket/string
         "handler_basic.rkt"
         "parser.rkt")


(define (text-handler code-file docu-file)

  (define (docu-preprocess c)
    (append (map (lambda (l)
                   (if (mode-docu? (chunk-mode c))
                       l
                       (string-append "  " l)))
              (default-writer-preprocess c))
            (list "")))

  (handler 
    (writer
      (default-writer-file code-file)
      default-code-writer-filter
      default-writer-preprocess)
    (writer
      (default-writer-file docu-file)
      default-docu-writer-filter
      docu-preprocess)))


(define (latex-handler code-file docu-file)

  (define (docu-preprocess c)
    (if (mode-code? (chunk-mode c))
      (append (list (string-replace "\\%lstlisting}" "%" "begin{"))
              (default-writer-preprocess c)
              (list (string-replace "\\%lstlisting}" "%" "end{")))
      (default-writer-preprocess c)))

  (handler 
    (writer
      (default-writer-file code-file)
      default-code-writer-filter
      default-writer-preprocess)
    (writer
      (default-writer-file docu-file)
      default-docu-writer-filter
      docu-preprocess)))


(define *HANDLERS*
  `(("text" ,text-handler)
    ("latex" ,latex-handler)))
  


(define (string->handler s)
  (let ((handler (assoc s *HANDLERS*)))
    (unless handler
            (error "Unsupported handler:" s))
    (cadr handler)))


(define (process-chunks h cs)
  (writer-write (handler-code-writer h) cs)
  (writer-write (handler-docu-writer h) cs))


(provide text-handler string->handler process-chunks)
