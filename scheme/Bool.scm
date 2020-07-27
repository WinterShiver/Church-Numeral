;; Bool.scm
;; Expressing boolean values and logics in a functional manner

(load "Base.scm")

;; values
;; functionally definiting boolean values: true, false
(define true const) ;; (define true (lambda (x y) x))
(define false (flip const)) ;; (define false (lambda (x y) y))


;; convert
;; convert between boolean values (b values, like #t or #f) 
;; and functional boolean values (fb values, like true or false)

;; toBool: fb 2 b
(define (toBool fb) (fb #t #f))
;; fromBool: b 2 fb
(define (fromBool b) (if b true false))