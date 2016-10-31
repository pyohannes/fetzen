;;```((mode docu) (post ("replace-variables")))
;;\section{\tc{``(file-basename)``}}
;;
;;This module holds the definition of all internal data structures and
;;operations on those data structures. The main data structure is called a
;;\tc{chunk}. It holds a list of consecutive lines of fetzen source which
;;share the same processing rules. In a simple use case a fetzen source
;;file will be split into \tc{chunk}s of lines that represent either code
;;or documentation.
;;
;;The \tc{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The most basic data structure is the \tc{line}. It holds the text of a
;;line of fetzen source code in the \tc{text} attribute and the
;;corresponding line number in the \tc{number} attribute:
;;```
(struct line (text number))
;;```
;;A \tc{chunk} holds a the attributes \tc{lines}, \tc{filename} and
;;\tc{rules}:
;;```
(struct chunk ([lines #:mutable] filename rules))
;;```
;;The attributes \tc{lines} holds a list of lines, the name of the
;;file the lines originate from is saved in \tc{filename}. The \tc{rules} 
;;attribute holds a hash table that contains rules for processing the
;;chunk. The hash table contains symbols as keys and strings as values.
;;
;;Finally two utility functions for working with \tc{chunk}s are provided.
;;The function \tc{chunk-append-line} appends a line \tc{l} to the list of lines
;;in a chunk \tc{c}. The attribute \tc{lines} has to be rebound to a
;;new list comprising the old list of lines and the new line \tc{l}:
;;```
(define (chunk-append-line c l)
  (set-chunk-lines! c (append (chunk-lines c) (list l))))
;;```
;;Very often one has to determine the mode of a \tc{chunk}. The mode of a 
;;\tc{chunk} determines how a \tc{chunk} is processed. The default
;;\tc{fetzen} postprocessors know the \tc{code} and the \tc{documentation}
;;modes. Custom modes can be defined and used by custom postprocessors.
;;
;;The mode is stored as an entry in the \tc{rules} hash table. The utility
;;function \tc{chunk-mode} retrieves the value for the key
;;\tc{'mode} from the hash table:
;;```
(define (chunk-mode c)
  (hash-ref (chunk-rules c) 'mode))
;;```
;;All bindings from this module are made available:
;;```
(provide (all-defined-out))
