#lang rosette/safe
(require rosette/lib/synthax)
(require racket/dict)
(require racket/match)
(require racket/string)
(require rosette/lib/angelic)
(require "../program_representation/concurrent-object-lib.rkt")
(require "../program_representation/simulator-structures.rkt")

(define xssss (choose* 1 2 3))
(synthesize #:forall (list)
            #:guarantee (assert (> xssss 5)))

