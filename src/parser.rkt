;;```((mode docu) (post ("replace-variables")))
;;\section{\textit{``(file-basename)``}}
;;
;;\subsection{Structure of \textit{fetzen} files}
;;Every line in a \textit{fetzen} file falls in one of three categories:
;;comment lines, rule lines or \textit{fetzen} source lines.
;;\subsubsection*{Comments}
;;Comment lines are lines that start with three hashes. 
;;Those lines are skipped by the parser and do not affect the operation and
;;output of \textit{fetzen}. This is an example for a comment line:
;;\begin{lstlisting}
;; ### This is a comment.
;;\end{lstlisting}
;;\subsubsection*{Rules}
;;Rule lines are lines that start with
;;three back ticks. Those lines are used to partition the source lines into
;;chunks. A rule lines indicates the beginning of a new chunk
;;containing all the lines following the rule line until the next rule line.
;;
;;Additional information on those lines represents rules that can alter the way the chunk is 
;;postprocessed. This rules are given in the form of an 
;;association list that can be parsed by the Scheme \textit{read} function.
;;This associated list is of the following form:
;;\begin{lstlisting}
;;((key value) ...)
;;\end{lstlisting}
;;\textit{key} must be a symbol.  The following keys are supported by 
;;\textit{fetzen}:
;;\begin{itemize}
;;\item \textit{mode}: The mode of a chunk hints the way it should be
;;postprocessed. \textit{fetzen} supports the two modes \textit{code} for
;;chunks that should be processed as code and \textit{docu} for chunks that
;;should be processed as documentation. User defined modes can be used by user
;;defined postprocessors.
;;\item \textit{post}: With the \textit{post} key a list of postprocessors can
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
;;furthermore specifies that the postprocessor called \textit{replace-variables} is
;;applied to the chunk.
;;
;;All rules are optional. If no postprocessors are given, only postprocessors
;;given via the command line are applied. If no mode is given, then the mode
;;\textit{docu} is assumed if the previous chunk had the mode set to
;;\textit{code}, otherwise the mode \textit{code} is assumed. If no mode is
;;given for the first chunk of a file it is assumed to be \textit{docu}.
;;
;;So if no modes are given in rule lines in a file at all, the \textit{fetzen} 
;;parser parses the file into chunks of alternating code and documentation
;;chunks, starting with a documentation chunk. The following example shows a
;;very simple \textit{fetzen} source file:
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
;;\subsubsection*{\textit{fetzen} source}
;;All lines that are neither comment lines nor rule lines are considered source
;;lines. Those lines are assigned to the chunk started by the last rule line.
;;\subsection{Implementation of the parser}
;;
;;The \textit{racket/base} library is used:
;;```
#lang racket/base
;;```
;;The function \textit{file$\rightarrow$lines} from the racket module 
;;\textit{file} is used to read the \textit{fetzen} source file, the racket
;;module \textit{racket/list} is required due to the use of the \textit{range}
;;function. The \textit{startswith?} function from \textit{utils.rkt} is used
;;to check if lines are comments or rules.
;;\textit{string$\rightarrow$preprocessor} from \textit{preprocessors.rkt} is
;;used to obtain preprocessor functions that are then applied to the raw
;;source lines. Finally the parsed data is read into
;;the data structures \textit{chunk} and \textit{line} defined in 
;;\textit{data.rkt}.
;;```
(require racket/file
         racket/list
         "utils.rkt"
         "preprocessors.rkt"
         "data.rkt")

;;```
;;Next some string constants are defined. Those constans define the basic 
;;elements of \textit{fetzen} files and rules.
;;```
(define <COMMENT-PREFIX> "###")
(define <RULE-ID> "```")
(define <MODE-ID> 'mode)
(define <CODE-ID> 'code)
(define <DOCU-ID> 'docu)
(define <POST-ID> 'post)


(define (line-comment? l)
  (startswith? (line-text l) <COMMENT-PREFIX>))


(define (line-instruction? l)
  (startswith? (line-text l) <RULE-ID>))


(define (file->enumerated-lines filename)
  (let ([lines (file->lines filename)])
    (map
      line
      lines (range 1 (+ (length lines) 1)))))


(define (line->chunk l filename old-chunk)

  (define (reverse-mode old-mode)
    (if (equal? old-mode <CODE-ID>)
        <DOCU-ID>
        <CODE-ID>))

  (chunk
    '()
    filename
    (apply hash (append (list <MODE-ID> (reverse-mode (chunk-mode old-chunk)))
                        (let ([instr (read (open-input-string (substring (line-text l) 3)))])
                          (if (list? instr)
                              (flatten instr)
                              '()))))))


(define (file->chunks filename pp)

  (define (lines->chunks lines c)
    (if (null? lines)
        (list c)
        (let ((l (car lines))
              (rest (cdr lines))) 
          (cond ((line-comment? l)
                 (lines->chunks rest c))
                ((line-instruction? l)
                 (cons c
                   (lines->chunks
                     rest
                     (line->chunk l filename c))))
                (else
                  (chunk-append-line c l)
                  (lines->chunks rest c))))))

  (let ([composed-preprocessor (apply compose1 (map string->preprocessor pp))])
    (lines->chunks 
      (composed-preprocessor (file->enumerated-lines filename))
      (chunk '() filename (hash <MODE-ID> <DOCU-ID>)))))


(provide file->chunks)
