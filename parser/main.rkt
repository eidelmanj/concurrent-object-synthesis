#lang racket

(require racket/file
         "parser.rkt")

(define (handle-file filename)
  (let ([file-text (file->string filename)])
    (let ([input (open-input-string file-text)])
      (display (pp (simple-math-parser (lex-this simple-math-lexer input)))))))


(define file-to-compile
  (command-line
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   (handle-file filename)))


