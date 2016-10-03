;;\section{\textit{handlers.rkt}}
;;```
#lang racket/base


(require racket/string
         "handler_basic.rkt"
         "parser.rkt")


(define (text-handler code-file docu-file)

  (define (docu-postprocess c)
    (append (map (lambda (l)
                   (if (mode-docu? (chunk-mode c))
                       l
                       (string-append "  " l)))
              (default-writer-postprocess c))
            (list "")))

  (handler 
    (writer
      (default-writer-file code-file)
      default-code-writer-filter
      default-writer-postprocess)
    (writer
      (default-writer-file docu-file)
      default-docu-writer-filter
      docu-postprocess)))


(define (latex-handler code-file docu-file)

  (define (docu-postprocess c)
    (if (mode-code? (chunk-mode c))
      (append (list (string-replace "\\%lstlisting}" "%" "begin{"))
              (default-writer-postprocess c)
              (list (string-replace "\\%lstlisting}" "%" "end{")))
      (default-writer-postprocess c)))

  (handler 
    (writer
      (default-writer-file code-file)
      default-code-writer-filter
      default-writer-postprocess)
    (writer
      (default-writer-file docu-file)
      default-docu-writer-filter
      docu-postprocess)))


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
