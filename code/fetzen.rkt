#lang racket/base


(require racket/cmdline
         "parser.rkt"
         "handlers.rkt")


(define code-file (make-parameter "fetzen.code"))
(define docu-file (make-parameter "fetzen.documentation"))
(define src-file  (make-parameter "fetzen.fet"))
(define handler-d (make-parameter "text"))


(command-line
  #:program "fetzen"
  #:once-each
  [("-c" "--code") 
   code 
   "Output file for code"
   (code-file code)]
  [("-d" "--documentation")
   docu
   "Output file for documentation"
   (docu-file docu)]
  [("--handler")
   h
   "Supported output handlers: text, latex."
   (handler-d h)]
  #:args (filename)
  (src-file filename))


(define (main)
  (let ((handler ((string->handler (handler-d)) (code-file) (docu-file))))
    (process-chunks handler (file->chunks (src-file)))))


(main)
