;; Nat.scm
;; Expressing natural numbers and logics on them in a functional manner
;; The experssion scheme is based on Church Numeral: 
;; https://karczmarczuk.users.greyc.fr/Essays/church.html

(load "Base.scm")

;; values
;; functionally defining natural numbers by `zero` and `incr` 
;; values construected by `zero` and `incr` are called nat values (nats)

(define zero
  (lambda (f x) x)
) 
;; or:
;; (define zero (flip const))

(define (incr nat)
  (lambda (f x) (nat f (f x)))
)
;; or:
;; (define (incr nat)
;;   (lambda (f x) (f (nat f x)))
;; )


;; transform
;; transform between nat values and natural numbers

;; fromNat: nat 2 int
(define (1+ n) (+ n 1))
(define (fromNat nat) (nat 1+ 0))
;; toNat: int 2 nat
(define (toNat n) 
  (if (> n 0) 
    (incr (toNat (- n 1))) 
    zero
  )
) 

;; useful notations

(define one (incr zero))
(define two (incr one))
(define three (incr two))
(define four (incr three))
(define five (incr four))
(define six (incr five))
(define seven (incr six))
(define eight (incr seven))
(define nine (incr eight))
(define ten (incr nine))
(define eleven (incr ten))
(define twelve (incr eleven))