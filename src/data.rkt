;;```((mode docu) (post ("replace-variables")))
;;\section{\textit{``(file-basename)``}}
;;
;;This module holds the definition of all internal data structures and
;;operations on those data structures. The main data structure is called a
;;\textit{chunk}. It holds a list of consecutive lines of fetzen source which
;;share the same processing rules. In a simple use case a fetzen source
;;file will be split into \textit{chunk}s of lines that represent either code
;;or documentation.
;;
;;The \textit{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The most basic data structure is the \textit{line}. It holds the text of a
;;line of fetzen source code in the \textit{text} attribute and the
;;corresponding line number in the \textit{number} attribute:
;;```
(struct line (text number))
;;```
;;A \textit{chunk} holds a the attributes \textit{lines}, \textit{filename} and
;;\textit{rules}:
;;```
(struct chunk ([lines #:mutable] filename rules))
;;```
;;The attributes \textit{lines} holds a list of lines, the name of the
;;file the lines originate from is saved in \textit{filename}. The \textit{rules} 
;;attribute holds a hash table that contains rules for processing the
;;chunk. The hash table contains symbols as keys and strings as values.
;;
;;Finally two utility functions for working with \textit{chunk}s are provided.
;;The function \textit{chunk-append-line} appends a line \textit{l} to the list of lines
;;in a chunk \textit{c}. The attribute \textit{lines} has to be rebound to a
;;new list comprising the old list of lines and the new line \textit{l}:
;;```
(define (chunk-append-line c l)
  (set-chunk-lines! c (append (chunk-lines c) (list l))))
;;```
;;Very often one has to determine the mode of a \textit{chunk}. The mode of a 
;;\textit{chunk} determines how a \textit{chunk} is processed. The default
;;\textit{fetzen} postprocessors know the \textit{code} and the \textit{documentation}
;;modes. Custom modes can be defined and used by custom postprocessors.
;;
;;The mode is stored as an entry in the \textit{rules} hash table. The utility
;;function \textit{chunk-mode} retrieves the value for the key
;;\textit{'mode} from the hash table:
;;```
(define (chunk-mode c)
  (hash-ref (chunk-rules c) 'mode))
;;```
;;All bindings from this module are made available:
;;```
(provide (all-defined-out))
