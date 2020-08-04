;; Utils

(define id (lambda (x) x))

(define cpsNothing 
  (lambda (ks)
    (lambda (kf) kf)
  )
)

(define (cpsJust sth)
  (lambda (ks)
    (lambda (kf) (ks sth))
  )
)

;; Basic Definition

(define zero
  (lambda (s)
    (lambda (z) z)
  )
)

(define (incr ch)
  (lambda (s)
    (lambda (z) 
      (s ((ch s) z))
    )
  )
)

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

(define (toInt ch)
  ((ch (lambda (x) (+ x 1))) 0)
)

;; Defining decr corresponding to decr'1c in Pred.hs

(define (incr1c c)
  (cpsJust ((c incr) zero))
) 
;; incr1c cpsNothing = cpsJust zero, incr1c (cpsJust zero) = cpsJust one, ..

(define (decr1c ch)
  ((ch incr1c) cpsNothing)
) 
;; decr1c zero = cpsNothing, decr1c one = cpsJust zero, ..

(define (decr1 ch)
  (((decr1c ch) id) zero)
) 
;; decr1 zero = zero, decr1 one = zero, decr1 two = one, ..