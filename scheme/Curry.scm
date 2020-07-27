;; curry: from f(a, b) to f'(a)(b)
;;   in which f' is a highly-ordered function, inputing a parameter,
;;   outputing a partial applied function.
;; uncurry: from f'(a)(b) to f(a, b)
;;   the inverse operation of curry.

(define (curry f)
  (lambda (a)
    (lambda (b) (f a b))
  )
)

(define (uncurry f)
  (lambda (a b) ((f a) b))
)

;; props: 
;; (curry (uncurry f)) == f == (uncurry (curry f))