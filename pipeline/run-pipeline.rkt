#lang racket


(define all-args  (vector->list (current-command-line-arguments)))

(cond
  [(not (= (length all-args) 3))
      (displayln "Usage: run-pipeline.rkt extension-file sketch-background-file concurrent-lib")
      (exit)])


(define extension-file (first all-args))
(define sketch-background-file (second all-args))
(define concurrent-lib (third all-args))


(define extension-contents (file->lines extension-file))
(define extension-no-meta-info (rest extension-contents))

(define concurrent-lib-str (file->string concurrent-lib))




(define (find-sketch-beginning lines)
  (cond
    [(empty? lines) `()]
    [(and
      (regexp-match-positions #rx"void" (first lines))
      (regexp-match-positions #rx"SKETCH" (first lines)))
     lines]
    [else
     (find-sketch-beginning (rest lines))]))

(define (until-end-of-sketch lines counter)
  (cond
    [(empty? lines) `()]
    [(regexp-match-positions #rx"{" (first lines))
     (display "adding 1 to ")(displayln counter)
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) (+ counter 1)))]
    [(and
      (regexp-match-positions #rx"}" (first lines))
      (= counter 1))
     (displayln "finished method!")

      (list (first lines))]
    [(regexp-match-positions #rx"}" (first lines))
     (display "subtracting 1 from ") (displayln counter)
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) (- counter 1)))]
    [else
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) counter))]))



    

(define sketch-too-long (find-sketch-beginning (file->lines "outComplete2.cpp")))
(until-end-of-sketch sketch-too-long 0)



(system (string-append "cp " sketch-background-file " ../c++/background.sk"))


;; (system (string-append "cd ../c++ && cat " extension-file " | ./seq_search > ../pipeline/out_log.txt"))

(system "cp ../c++/outComplete*.cpp ./")











