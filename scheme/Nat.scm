;; Nat.scm
;; Expressing natural numbers and logics on them in a functional manner
;; The experssion scheme is based on Church Numeral: 
;; https://karczmarczuk.users.greyc.fr/Essays/church.html

(load "Base.scm")

;; values
;; functionally defining natural numbers by `zero` and `succ` 
;; values construected by `zero` and `succ` are called nat values (nats)

(define zero
  (lambda (f x) x)
) 
;; or:
;; (define zero (flip const))

(define (succ nat)
  (lambda (f x) (nat f (f x)))
)
;; or:
;; (define (succ nat)
;;   (lambda (f x) (f (nat f x)))
;; )


;; convert
;; convert between nat values and natural numbers

;; fromNat: nat 2 int
(define (1+ n) (+ n 1))
(define (fromNat nat) (nat 1+ 0))
;; toNat: int 2 nat
(define (toNat n) 
  (if (> n 0) 
    (succ (toNat (- n 1))) 
    zero
  )
) 

;; useful notations

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))
(define eleven (succ ten))
(define twelve (succ eleven))