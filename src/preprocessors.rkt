;;```((mode docu) (post ("replace-variables")))
;;\section{\tc{``(file-basename)``}}
;;
;;Preprocessors are applied to the lines of fetzen source file before they are
;;grouped into chunks. One main purpose of preprocessors is removing comments
;;from documentation parts. It is very convenient to comment out documentation 
;;parts in the fetzen source - in this way fetzen source files can directly
;;compiled without running them through fetzen. A preprocessor can be used to
;;uncomment the lines containing documentation and fetzen rules so the source
;;can be processed by fetzen.
;;
;;The \tc{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The module \tc{utils.rkt} is needed for the \tc{startswith?}
;;function. The data structure \tc{line} is used from the module
;;\tc{data.rkt}:
;;```
(require "utils.rkt"
         "data.rkt")
;;```
;;The function \tc{uncomment-lines-start} makes the definition of other
;;function that remove comments from the beginning of lines more convenient.
;;This function accepts an argument \tc{pattern}. It returns a function
;;that accepts a list of lines as argument and that removes the string in
;;\tc{pattern} from the beginning of every line:
;;```
(define (uncomment-lines-start pattern)
;;```
;;\tc{uncomment-single-line} is a helper function that accomplishes the
;;task for a single line. If the line \tc{l} starts with the string in
;;\tc{pattern} a substring not containing \tc{pattern} is used to
;;construct a new \tc{line} object:
;;```
  (define (uncomment-single-line l)
    (let* ([orig (line-text l)]
           [text (if (startswith? orig pattern)
                     (substring orig (string-length pattern))
                     orig)])
      (struct-copy line l (text text))))
;;```
;;Finally a function returned  that accepts a list of \tc{line}s as 
;;argument and that applies the helper function defined above to every 
;;\tc{line} in the list:
;;```
  (lambda (lines)
    (map (lambda (l)
           (uncomment-single-line l))
         lines)))
;;```
;;The user is not granted direct access to the preprocessor functions in this
;;module. The user specifies preprocessor via string identifiers on the command
;;line. The function \tc{string$\rightarrow$preprocessor} returns the preprocessor
;;function for a string identifier. The mapping of string identifiers to
;;functions is stored in the \tc{*preprocessors*} hash table:
;;```
(define *preprocessors*
  (hash "uncomment-;;" (uncomment-lines-start ";;")))
;;```
;;The function \tc{string$\rightarrow$preprocessor} returns the preprocessor function
;;for the preprocessor string identifier given in \tc{s}. If no matching
;;entry in the hash table exists an error is raised:
;;```
(define (string->preprocessor s)
  (if (hash-has-key? *preprocessors* s)
      (hash-ref *preprocessors* s)
      (error "Unsupported preprocessor: " s)))
;;```
;;From this module the function \tc{string$\rightarrow$preprocessor} and the hash
;;table \tc{*preprocessors*} are exported. The user can extend this hash
;;table by user defined preprocessors.
;;```
(provide string->preprocessor *preprocessors*)
