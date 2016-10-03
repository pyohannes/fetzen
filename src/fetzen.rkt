;;\section{\textit{fetzen.rkt}}
;;```
#lang racket/base


(require racket/cmdline
         racket/file
         "parser.rkt"
         "handlers.rkt"
         "preprocessors.rkt")


(define code-file (make-parameter "fetzen.code"))
(define docu-file (make-parameter "fetzen.documentation"))
(define src-file  (make-parameter "fetzen.fet"))
(define handler-d (make-parameter "text"))
(define preproc-d (make-parameter "none"))


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
  [("--preprocessor")
   h
   "Applies the named preprocessor to the source code."
   (preproc-d h)]
  #:args (filename)
  (src-file filename))


(define (main)
  (let ((handler ((string->handler (handler-d)) (code-file) (docu-file)))
        (preprocess (string->preprocessor (preproc-d))))
    (process-chunks handler 
                    (lines->chunks (preprocess (file->lines (src-file)))))))


(main)
