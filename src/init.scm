
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))
(define (caaaar x) (caar (caar x)))
(define (caaadr x) (caar (cadr x)))
(define (caadar x) (caar (cdar x)))
(define (caaddr x) (caar (cddr x)))
(define (cadaar x) (cadr (caar x)))
(define (cadadr x) (cadr (cadr x)))
(define (caddar x) (cadr (cdar x)))
(define (cadddr x) (cadr (cddr x)))
(define (cdaaar x) (cdar (caar x)))
(define (cdaadr x) (cdar (cadr x)))
(define (cdadar x) (cdar (cdar x)))
(define (cdaddr x) (cdar (cddr x)))
(define (cddaar x) (cddr (caar x)))
(define (cddadr x) (cddr (cadr x)))
(define (cdddar x) (cddr (cdar x)))
(define (cddddr x) (cddr (cddr x)))

(define (list? e)
  (or (null? e)
      (and (pair? e)
           (list? (cdr e)))))

(define list (lambda args args))

(define (not x) (if x #f #t))

(define (fraction? q)
  (and (rational? q)
       (not (integer? q))))


(define throw-error (lambda () (error 'some-bin-func "bad numbers bro come'on")))

(define bin+
           (lambda (a b)
             (cond ((rational? a)
                    (cond ((rational? b) (__bin-add-qq a b))
                          ((real? b) (__bin-add-rr (rational->real a) b))
                          (else (throw-error))))
                   ((real? a)
                    (cond ((rational? b) (__bin-add-rr a (rational->real b)))
                          ((real? b) (__bin-add-rr a b))
                          (else (throw-error))))
                   (else (throw-error)))))

(define list+
  (lambda (s)
    (if (null? s) 0
        (bin+ (car s) (list+ (cdr s)))
    )
  )
)

(define + 
  (lambda s
    (list+ s)
  )
)

(define bin-
           (lambda (a b)
             (cond ((rational? a)
                    (cond ((rational? b) (__bin-sub-qq a b))
                          ((real? b) (__bin-sub-rr (rational->real a) b))
                          (else (throw-error))))
                   ((real? a)
                    (cond ((rational? b) (__bin-sub-rr a (rational->real b)))
                          ((real? b) (__bin-sub-rr a b))
                          (else (throw-error))))
                   (else (throw-error)))))

(define list-
  (lambda (s)
    (if (null? s) 0
        (bin- (car s) (list+ (cdr s)))
    )
  )
)

(define -
  (lambda s
    (list- s)
  )
)


(define bin*
           (lambda (a b)
             (cond ((rational? a)
                    (cond ((rational? b) (__bin-mul-qq a b))
                          ((real? b) (__bin-mul-rr (rational->real a) b))
                          (else (throw-error))))
                   ((real? a)
                    (cond ((rational? b) (__bin-mul-rr a (rational->real b)))
                          ((real? b) (__bin-mul-rr a b))
                          (else (throw-error))))
                   (else (throw-error)))))
(define list*
  (lambda (s)
    (if (null? s) 1
        (bin* (car s) (list* (cdr s)))
    )
  )
)

(define * 
  (lambda s
    (list* s)
  )
)

(define bin/
           (lambda (a b)
             (cond ((rational? a)
                    (cond ((rational? b) (__bin-div-qq a b))
                          ((real? b) (__bin-div-rr (rational->real a) b))
                          (else (throw-error))))
                   ((real? a)
                    (cond ((rational? b) (__bin-div-rr a (rational->real b)))
                          ((real? b) (__bin-div-rr a b))
                          (else (throw-error))))
                   (else (throw-error)))))

(define list/
  (lambda (s)
    (if (null? s) 1
        (bin/ (car s) (list* (cdr s)))
    )
  )
)

(define /
  (lambda s
    (list/ s)
  )
)

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (bin* n (fact (- n 1))))))


(define apply __bin-apply)

(define ormap-list
  (lambda (f s)
    (if (null? s) #f
      (if (car s) (car s)
        (ormap-list f (cdr s))
      )
    )
  )
)

(define ormap
  (lambda (f . s)
    (ormap-list f s) 
  )
)


(define andmap-list
  (lambda (f s)
    (if (null? s) #t
      (if (not (car s)) (car s)
        (andmap-list f (cdr s))
      )
    )
  )
)

(define andmap
  (lambda (f . s)
    (andmap-list f s) 
  )
)
