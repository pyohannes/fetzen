\section{\textit{handler.rkt}}

In this module we use the base racket language. All identifiers defined in this 
module are exported.
```
#lang racket/base

(provide (all-defined-out))
```
The handler deals with chunks it receives from the parser and therefore uses 
functionality from the parser to handle chunks, like \textit{chunk-mode} or 
\textit{chunk-lines}.

As with all modules, we use the base racket language.
```
(require "parser.rkt")
```
The basic definition of the writer is as follows:
```
(struct writer (file filter preprocess))
```
Firstly a \textit{writer} references a certain \textit{file}, which is its output 
file. All chunks given to the \textit{writer} have to pass the \textit{filter} to 
be actually written into the file. Chunks that don't pass the \textit{filter} are 
omitted. Finally the \textit{preprocess} function can do modifications to the 
content of a chunk before it is written into the file.

This handler module also provides defaults that can be used for the 
\textit{file}, \textit{filter} or \textit{preprocess} members of a writer.

The \textit{default-writer-file} function creates a default for the file member. 
In this case it is just a simple \textit{racket} output file object. The file 
is replaced when it already exists.
```
(define (default-writer-file fname)
  (open-output-file fname #:exists 'replace))
```
Two default filters are provided: one for the code file and one for the 
documentation file.

The default filter for the code file tests, if the given chunk contains code. 
Non-code chunks are omitted in code files.
```
(define (default-code-writer-filter c)
  (mode-code? (chunk-mode c)))
```
The default filter for the documentation file returns \textit{\#t} for every chunk.
```
(define (default-docu-writer-filter c)
  #t)
```
The default \textit{preprocess} method simply gives the lines from the chunk as
they are without any modifications.
```
(define (default-writer-preprocess c)
  (chunk-lines c))
```
A list of chunks \textit{chunks} can be passed to a writer \textit{t} via the 
\textit{writer-write} function.
```
(define (writer-write w chunks)
```
A helper function \textit{write-lines} writes a list of lines to the file of the
writer. A line break is added to every line:
```
  (define (write-lines lines)
    (map (lambda (l)
           (fprintf (writer-file w) "~a~n" l))
         lines))
```
Finally the \textit{write-lines} function is mapped to the filtered and 
preprocessed lines of the given chunk:
```
  (for-each write-lines
    (map (writer-preprocess w)
         (filter (writer-filter w) chunks))))
```
A \textit{handler} consists of nothing more than two writers: a 
\textit{code-writer} for writing to the code file and a \textit{docu-writer} for 
writing to the documentation file.
```
(struct handler (code-writer docu-writer))
