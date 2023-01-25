; (define test
;     (lambda (x . s) s)
; )

; (test 1 2 3)

(define inc (lambda (x) (+ x 1)))
(define id (lambda (x) x))

; (andmap id #t #f 2)
; (reverse '(1 2 3))


; (map inc 1 2)

; (prepand '(1 2 3) 5)

(fold-left - 3 2 1)

