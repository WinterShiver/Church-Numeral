;; Operators of fb values 

(load "Base.scm")
(load "Bool.scm")

;; unary ops: not

(define not flip)

;; binary ops: and, or, xor

(define (and fb1 fb2) (fb1 fb2 false))
(define (or fb1 fb2) (fb1 true fb2))
(define (xor fb1 fb2) (fb1 (not fb2) fb2))

