(define lambda-variadic ((lambda a (quasiquote (variadic lambda on list ,@a))) 1 2))

(define lambda-opt ((lambda (a . b) (quasiquote (lambda opt arg ,a rest ,@b))) 6 9))

(define plus ((lambda (a b) (quasiquote (binary plus performing ,a + ,b result is ,(bin+ a b)))) 5 4))

(define set-example (let ((x 2)) (begin (set! x 3) (quasiquote (set bang x was 2 set to ,x)))))    
(define set-example-in-lambda (lambda () (define nadav 0) (set! nadav (quasiquote (set bang in lambda works))) nadav))

(define sprint-list (lambda (l) (if (null? l) (quasiquote (letrec macro expansion works)) (sprint-list (cdr l)))))  
(define letrec-no-let (sprint-list '(1 2 3)))

(define fact (lambda (n) (if (zero? n) 1 (bin* n (fact (bin- n 1))))))
(define fact-example (quasiquote (fact of 3 ,(fact 3))))

(let ((should-test #f)
    (divider '_____________________________________________________________________________________________________________________________________________) (space '______))
(if (not should-test)   
        '()
        (quasiquote (
            ,divider
            ,lambda-variadic ,space
            ,lambda-opt ,space
            ,plus ,space
            ,fact-example ,space
            ,set-example ,space
            ,(set-example-in-lambda) ,space
            ,letrec-no-let
            )))
)



; ******    letrec recursions don't work
(define letrec-example (lambda ()  
    (letrec ((sprint-list (lambda (l) (if (null? l) (quasiquote (letrec works)) (sprint-list (cdr l)))))) (sprint-list '(1 2 3)))))
(letrec-example) ; supposed to print "letrec works" but instead throws exception  *********
