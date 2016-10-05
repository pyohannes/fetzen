#lang racket/base


(require rackunit
         rackunit/text-ui
         racket/string
         racket/path
         racket/file
         racket/system)


(define (file-join . paths)
  (string-join paths "/"))


(define (make-temporary-dir)
  (let ((path (make-temporary-file)))
    (delete-file path)
    (make-directory* path)
    path))


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
    (let* ((testdir  (file-join *TESTDATADIR* dir))
           (src      (file-join testdir "fetzen"))
           (argsfile (file-join testdir "arguments"))
           (tempdir  (path->string (make-temporary-dir)))
           (args     (string-split
                       (string-replace (file->string argsfile)
                                       "<DIR>"
                                       tempdir))))
      (apply system* `("fetzen" ,@args ,src))
      (for-each 
        (lambda (fname)
          (unless (member fname '("fetzen" "arguments"))
            (test-file-content-equal? (file-join testdir fname)
                                      (file-join tempdir fname))))
        (map path->string (directory-list testdir))))))


(run-tests fetzen-files)
