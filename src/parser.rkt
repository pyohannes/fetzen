;;```((mode docu) (post ("replace-variables")))
;;\section{\tc{``(file-basename)``}}
;;
;;\subsection{Structure of \tc{fetzen} files}
;;Every line in a \tc{fetzen} file falls in one of three categories:
;;comment lines, rule lines or \tc{fetzen} source lines.
;;\subsubsection*{Comments}
;;Comment lines are lines that start with three hashes. 
;;Those lines are skipped by the parser and do not affect the operation and
;;output of \tc{fetzen}. This is an example for a comment line:
;;\begin{lstlisting}
;; ### This is a comment.
;;\end{lstlisting}
;;\subsubsection*{Rules}
;;Rule lines are lines that start with
;;three back ticks. Those lines are used to partition the source lines into
;;chunks. A rule lines indicates the beginning of a new chunk
;;containing all the lines following the rule line until the next rule line or
;;the end of the file.
;;
;;Additional information on those lines represents rules that can alter the way the chunk is 
;;postprocessed. This rules are given in the form of an 
;;association list that can be parsed by the Scheme \tc{read} function.
;;This associated list is of the following form:
;;\begin{lstlisting}
;;((key value) ...)
;;\end{lstlisting}
;;\tc{key} must be a symbol.  The following keys are supported by 
;;\tc{fetzen}:
;;\begin{itemize}
;;\item \tc{mode}: The mode of a chunk hints the way it should be
;;postprocessed. \tc{fetzen} supports the two modes \tc{code} for
;;chunks that should be processed as code and \tc{docu} for chunks that
;;should be processed as documentation. User defined modes can be used by user
;;defined postprocessors.
;;\item \tc{post}: With the \tc{post} key a list of postprocessors can
;;be specified. The names of the postprocessors must be given as strings. The
;;mentioned postprocessors are applied to the chunk in addition to the
;;postprocessors specified via the command line. Postprocessors are discussed
;;in more detail below.
;;\end{itemize}
;;
;;This is an example for a rule line:
;;\begin{lstlisting}
;; ```((mode docu) (post ("replace-variables")))
;;\end{lstlisting}
;;In this example the rule line starts a chunk that holds documentation. It
;;furthermore specifies that the postprocessor called \tc{replace-variables} is
;;applied to the chunk.
;;
;;All rules are optional. If no postprocessors are given, only postprocessors
;;given via the command line are applied. If no mode is given, then the mode
;;\tc{docu} is assumed if the previous chunk had the mode set to
;;\tc{code}, otherwise the mode \tc{code} is assumed. If no mode is
;;given for the first chunk of a file it is assumed to be \tc{docu}.
;;
;;So if no modes are given in rule lines in a file at all, the \tc{fetzen} 
;;parser parses the file into chunks of alternating code and documentation
;;chunks, starting with a documentation chunk. The following example shows a
;;very simple \tc{fetzen} source file:
;;\begin{lstlisting}
;; The function below adds 1 to the argument given:
;; ```
;; (define (add1 a) (+ a 1))
;; ```
;; The function below adds 2 to the argument given:
;; ```
;; (define (add2 a) (+ a 2))
;; ```
;; Thank you for your attention.
;;\end{lstlisting}
;;
;;\subsubsection*{\tc{fetzen} source}
;;All lines that are neither comment lines nor rule lines are considered source
;;lines. Those lines are assigned to the chunk started by the last rule line.
;;\subsection{Implementation of the parser}
;;
;;The \tc{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The function \tc{file$\rightarrow$lines} from the racket module 
;;\tc{racket/file} is used to read the \tc{fetzen} source file, the racket
;;module \tc{racket/list} is required due to the use of the \tc{range}
;;function. The \tc{startswith?} function from \tc{utils.rkt} is used
;;to check if lines are comments or rules.
;;\tc{string$\rightarrow$preprocessor} from \tc{preprocessors.rkt} is
;;used to obtain preprocessor functions that are then applied to the raw
;;source lines. Finally the parsed data is read into
;;the data structures \tc{chunk} and \tc{line} defined in 
;;\tc{data.rkt}.
;;```
(require racket/file
         racket/list
         "utils.rkt"
         "preprocessors.rkt"
         "data.rkt")

;;```
;;Next some string constants are defined. Those constans define the basic 
;;elements of \tc{fetzen} files and rules as described above:
;;```
(define <COMMENT-PREFIX> "###")
(define <RULE-PREFIX> "```")
(define <MODE-KEY> 'mode)
(define <CODE-VALUE> 'code)
(define <DOCU-VALUE> 'docu)
(define <POST-KEY> 'post)
;;```
;;The next two predicates \tc{line-comment?} and \tc{line-rule?} help 
;;to determine if a line of a source file is a comment or a rule line. All 
;;lines for which both predicates are false is a \tc{fetzen} source line:
;;```
(define (line-comment? l)
  (startswith? (line-text l) <COMMENT-PREFIX>))

(define (line-rule? l)
  (startswith? (line-text l) <RULE-PREFIX>))
;;```
;;The rules on the rule line are given as an association list. To obtain a
;;Scheme object from the line firstly the rule prefix is stripped, then
;;secondly the text is piped through the \tc{read} function which returns
;;an association list: 
;;```
(define (rule-line->rules l)
  (read 
    (open-input-string 
      (substring (line-text l) 
                 (string-length <RULE-PREFIX>)))))
;;```
;;The function \tc{file$\rightarrow$enumerated-lines} builds on the racket
;;function \tc{file$\rightarrow$lines}. Whereas 
;;\tc{file$\rightarrow$lines} returns a list of strings, 
;;\tc{file$\rightarrow$enumerated-lines} returns a list of \tc{line} 
;;structs holding both the string and the corresponding line number:
;;```
(define (file->enumerated-lines filename)
  (let ([lines (file->lines filename)])
    (map
      line
      lines (range 1 (+ (length lines) 1)))))

;;```
;;Every rule line marks the beginning of a new chunk. Conveniently the function
;;\tc{rule$\rightarrow$chunk} initializes a new chunk based on a rule line.
;;The function relies on the fact that the passed line \tc{l} is a rule
;;line. Further arguments are the name of the file the line originates from in
;;\tc{filename} and the chunk preceding the new chunk in
;;\tc{pre-chunk}. The filename is needed because every chunk holds the name
;;of the file it was created from. The preceding chunk is needed to implement
;;the default alternation of code and documentation chunks when no mode is
;;explicitly given in the rule.
;;```
(define (rule->chunk l filename pre-chunk)
;;```
;;The little helper function \tc{reverse-mode} finally implements this
;;alteration: 
;;```
  (define (reverse-mode mode)
    (if (equal? mode <CODE-VALUE>)
        <DOCU-VALUE>
        <CODE-VALUE>))
;;```
;;An empty chunk pointing to the file \tc{filename} is initialized.
;;The rules in
;;the hash table are initialized from the rule line. The mode is set to the
;;default mode (the reversed mode from the previous chunk). This mode is
;;overriden if a mode is given in the rule line.
;;```
  (let ([default-mode (reverse-mode (chunk-mode pre-chunk))]
        [rules (rule-line->rules l)])
    (chunk
      '()
      filename
      (hash* (append (list <MODE-KEY> default-mode)
                     (if (list? rules)
                         (flatten rules)
                         '()))))))
;;```
;;The function \tc{file$\rightarrow$chunks} is the only function the parser
;;module exposes. In the argument \tc{filename} the name of a fetzen source
;;file is passed, in \tc{pp} a list of names of preprocessors is passed:
;;```
(define (file->chunks filename pp)
;;```
;;The helper function \tc{apply-preprocessors} obtains preprocessor functions
;;for preprocessor names in \tc{pp} and then applies all those functions to
;;every line in the \tc{lines} argument:
;;
;;```
  (define (apply-preprocessors lines)
    ((apply compose1 (map string->preprocessor pp)) lines))
;;```
;;The main work is done in the helper function \tc{lines$\rightarrow$chunks}.
;;It accepts a list of \tc{line} structs in \tc{lines} and a \tc{chunk} struct \tc{c} to 
;;start adding lines to. The function transforms all lines into a list of
;;chunks and returns this list. 
;;
;;Comment lines are skipped. For rule lines the current chunk struct \tc{c} is
;;added to the result and a new chunk is initialized via the
;;\tc{rule$\rightarrow$chunk} function. For all other lines (source code lines)
;;the line is added to the current chunk struct:
;;```
  (define (lines->chunks lines c)
    (if (null? lines)
        (list c)
        (let ((l (car lines))
              (rest (cdr lines))) 
          (cond ((line-comment? l)
                 (lines->chunks rest c))
                ((line-rule? l)
                 (cons c
                   (lines->chunks
                     rest
                     (rule->chunk l filename c))))
                (else
                  (chunk-append-line c l)
                  (lines->chunks rest c))))))
;;```
;;Finally the two helper functions \tc{apple-preprocessors} and
;;\tc{lines$\rightarrow$chunks} are combined. \tc{lines$\rightarrow$chunks} is
;;called with a list of \tc{line} structs that where transformed by the
;;\tc{apply-preprocessors} function. The initial chunk passed is an empty
;;documentation chunk, as the alternation of code and documentation chunks by
;;default starts with a documentation chunk. If the first line of a file is a
;;rule line this empty documentation chunk will precede the chunk initialized
;;by the first line. Issue \tc{\#18} was submitted for this misbehaviour.
;;```
  (lines->chunks
    (apply-preprocessors (file->enumerated-lines filename))
    (chunk '() filename (hash <MODE-KEY> <DOCU-VALUE>))))
;;```
;;Only the function \tc{file$\rightarrow$chunks} is exported:
;;```
(provide file->chunks)
