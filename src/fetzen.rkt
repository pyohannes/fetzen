;;\section{\textit{fetzen.rkt}}
;;```
#lang racket/base


(require racket/cmdline
         racket/string
         "parser.rkt"
         "writer.rkt")


(define out-files (make-parameter (list)))
(define preproc-d (make-parameter (list)))
(define src-file  (make-parameter ""))


(command-line
  #:program "fetzen"
  #:multi
  [("-p" "--preprocessor")
   pre 
   "Applies the named preprocessor to the source code."
   (preproc-d (cons pre (preproc-d)))]
  [("-o" "--out") 
   out
   "Give an output file and the related postprocessors, separated by commas."
   (out-files (cons (string-split out ",") (out-files)))]
  #:args (filename)
  (src-file filename))


(define (main)
  (process-chunks
    (map (lambda (out-file)
           (make-writer (car out-file) (cdr out-file)))
         (out-files))
    (file->chunks (src-file) (preproc-d))))


(main)
