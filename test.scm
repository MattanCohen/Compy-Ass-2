;;; recieves object a and improper list s
;;; returns a * s[0] * ... * s[len-2] . s[len-1]
(define run 
    (lambda (a s)
        (if (pair? s)
            (cons a
                (run (car s)
                    (cdr s)))
        a)))


;;;     f = procedure       s = list
;;;                         using run, make s proper list
;;;     using __bin_apply, apply f on the result of run
(define my-apply
    (lambda (f . s) 
        (__bin-apply f
            (run (car s)
                (cdr s)))))


; THIS TO PUT IN ASSembly
;
;;;     proc = procedure    w = proper list
;;;     run proc on every object in w

(define __bin-apply 
    (lambda (proc w) 
        (cond   ((and (pair? w) (pair? (cdr w)))
                    (proc (car w) (__bin-apply proc (cdr w))))
                ((pair? w)
                    (car w))
                (else '(big error in bin apply recieved a not list omg))
        )
    )
)



(define my-list '(1 2 3 4 5))

(define res1 (my-apply + 1 2 3 4 5))

(define res2 (my-apply cons 1 2 3 4 5))
