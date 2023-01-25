
(define lambda-variadic ((lambda a (quasiquote (variadic lambda on list ,@a))) 1 2))
(define lambda-opt ((lambda (a . b) (quasiquote (lambda opt arg ,a rest ,@b))) 6 9))
(define plus ((lambda (a b) (quasiquote (performing ,a + ,b result is ,(bin+ a b)))) 5 4))


(let ((space '___space___))
(quasiquote (
    ,lambda-variadic ,space
    ,lambda-opt ,space
    ,plus
    )))

; `((lambda-variadic 1 2 3 4))


; (bin* 1 0)
; (bin+ 2 4)
; (bin- 15 6)
; (bin/ 0 1)
