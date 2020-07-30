;; id

(define id
  (lambda (x) x)
)

;; props:
;; (id x) == x

;; flip

(define (flip f)
  (lambda (x)
    (lambda (y) ((f y) x))
  )
)

;; props:
;; (flip (flip f)) == f

;; const

(define const
  (lambda (x)
    (lambda (y) x)
  )
)

;; props:
;; (const id) == \x -> \y -> y == (filp const)

