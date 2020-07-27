;; Operators of functional natural numbers

(load "Curry.scm")
(load "Nat.scm")

;; utils
;; TODO: fold

;; binary ops: n and m stand for nat vars in below
;; add n m = n incr m
(define (add n m) (n incr m))
;; mul n m = n (add m) zero
(define (mul n m) (n ((curry add) m) zero))
;; pow n zero = one
;; pow n (incr m) = incr m n = (incr m) n
(define (pow n m)
  (case m
    ((zero) one)
    (else (uncurry ((curry m) (curry n))))
  )
)
