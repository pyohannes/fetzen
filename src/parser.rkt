;;\section{\textit{parser.rkt}}
;;```
#lang racket/base


(require racket/file
         racket/list
         "utils.rkt"
         "data.rkt"
         "preprocessors.rkt")


(define (line-comment? l)
  (startswith? (line-text l) "###"))


(define (line-instruction? l)
  (startswith? (line-text l) "```"))


(struct mode (code postprocessor-names))


(define (mode-docu-make)
  (mode 'docu '()))


(define (mode-code-make)
  (mode 'code '()))


(define (mode-code? mode)
  (eq? (mode-code mode) 'code))


(define (mode-docu? mode)
  (eq? (mode-code mode) 'docu))


(define (mode-code-reverse code)
  (if (eq? code 'code)
      'docu
      'code))


(define (line->chunk l filename old-chunk)

  (define (reverse-mode old-mode)
    (if (equal? old-mode "code")
      "documentation"
      "code"))

  (chunk
    '()
    filename
    (apply hash (append (list 'mode (reverse-mode (chunk-mode old-chunk)))
                        (let ([instr (read (open-input-string (substring (line-text l) 3)))])
                          (if (list? instr)
                              (flatten instr)
                              '()))))))


(define (line->mode l oldmode)

  (define (read-instruction text)
    (let ([instr (read (open-input-string (substring text 3)))])
      (if (list? instr)
          instr
          '())))

  (define (get-instruction instr key default)
    (let ([i (assoc key instr)])
      (if i
          (cadr i)
          default)))

  (let ([instr (read-instruction (line-text l))])
    (mode (get-instruction instr 'mode (mode-code-reverse (mode-code oldmode)))
          (get-instruction instr 'post '()))))


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
      (chunk '() filename (hash 'mode "documentation")))))


(define (file->enumerated-lines filename)
  (let ([lines (file->lines filename)])
    (map
      line
      lines (range 1 (+ (length lines) 1)))))


(provide file->chunks)
