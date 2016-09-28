#lang racket/base


(require rackunit
         rackunit/text-ui
         racket/string
         racket/path
         racket/file
         racket/system)


(define (file-join . paths)
  (string-join paths "/"))


(define (test-file-content-equal? src dst)
  (check-equal? (string-trim (file->string src) "\n")
                (string-trim (file->string dst) "\n")))


(define *TESTDATADIR*
  (file-join (path->string 
               (path-only (path->complete-path (find-system-path 'run-file))))
             "data"))


(define-test-suite fetzen-files
    (map create-test-for-directory 
         (map path->string (directory-list *TESTDATADIR*))))


(define (create-test-for-directory dir)
  (test-case (string-join `("Test for directory" ,dir) " ")
    (let* ((src (file-join *TESTDATADIR* dir "fetzen"))
           (ref-code (file-join *TESTDATADIR* dir "code"))
           (ref-docu (file-join *TESTDATADIR* dir "documentation"))
           (argsfile (file-join *TESTDATADIR* dir "arguments"))
           (args (if (file-exists? argsfile)
                     (string-split (file->string argsfile))
                     '()))
           (dst-code (make-temporary-file))
           (dst-docu (make-temporary-file)))
      (apply system* 
             (append '("fetzen")
                     args
                     `("--code" ,dst-code "--documentation" ,dst-docu ,src)))
      (test-file-content-equal? ref-code dst-code)
      (test-file-content-equal? ref-docu dst-docu))))


(run-tests fetzen-files)
