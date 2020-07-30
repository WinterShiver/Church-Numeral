;; Basic operations on functions
;; All functions declared in this file are curried.

;; application
;; appl: ($) in haskell, invAppl: (&) in haskell

(define appl
  (lambda (f)
    (lambda (x) (f x))
  )
)

(define invAppl (flip appl))

;; composition

(define comp
  (lambda (g) (lambda (f)
    (lambda (x) (g (f x)))
  ))
)

;; liftA2 of Applicative (-> r): liftA2 f g h = \x -> f (g x) (h x)

(define lift2 
  (lambda (f) (lambda (g) (lambda (h)
    (lambda (x) ((f (g x)) (h x)))
  )))
)
