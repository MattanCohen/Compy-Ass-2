
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
      (if (f (car s)) #t
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
      (if (f (car s)) (andmap-list f (cdr s))
        #f
      )
    )
  )
)

(define andmap
  (lambda (f . s)
    (andmap-list f s) 
  )
)



(define map-list
  (lambda (f s)
    (if (null? s) s
      (cons (f (car s)) (map-list f (cdr s)))
    )
  )
)

(define map
  (lambda (f . s)
    (map-list f s) 
  )
)


(define append-one 
  (lambda (l x)
    (if (null? l) (cons x l)
        (cons (car l) (append-one (cdr l) x))
    )
  )
)

(define append-list
  (lambda (l1 l2)
    (if (null? l2) l1
      (append-list (append-one l1 (car l2)) (cdr l2))
    )
  )
)

(define append
  (lambda (l . s)
    (append-list l s)
  )
)

(define prepand
  (lambda (l x)
    (cons x l)
  )
)

(define reverse-internal
  (lambda (new-l l)
    (if (null? l) new-l
      (reverse-internal (prepand new-l (car l)) (cdr l))
    )
  )
)

(define reverse
  (lambda (l)
    (reverse-internal '() l)
  )
)

(define fold-left-list
  (lambda (f unit ss)
    (if (null? ss) unit
      (fold-left-list f (f unit (car ss)) (cdr ss))
    )
  )
)

(define fold-left
  (lambda (f unit . ss)
    (fold-left-list f unit ss)
  )
)

(define fold-right-list
  (lambda (f unit ss)
    (fold-left-list f unit (reverse ss))
  )
)

(define fold-right
  (lambda (f unit . ss)
    (fold-left-list f unit ss)
  )
)

(define < #void)
(define <= #void)
(define > #void)
(define >= #void)
(define = #void)


; exit

; make-list


(define char<? #void)
(define char<=? #void)
(define char=? #void)
(define char>? #void)
(define char>=? #void)


(let ((make-char-comparator
        (lambda (comparator)
          (lambda s
            (apply comparator
              (map char->integer s))))))
  (set! char<? (make-char-comparator <))
  (set! char<=? (make-char-comparator <=))
  (set! char=? (make-char-comparator =))
  (set! char>? (make-char-comparator >))
  (set! char>=? (make-char-comparator >=)))

(define char-downcase #void)
(define char-upcase #void)

(let ((delta
        (- (char->integer #\a)
          (char->integer #\A))))
  (set! char-downcase
    (lambda (ch)
      (if (char<=? #\A ch #\Z)
          (integer->char
            (+ (char->integer ch) delta))
          ch)))
  (set! char-upcase
    (lambda (ch)
      (if (char<=? #\a ch #\z)
          (integer->char
            (- (char->integer ch) delta))
          ch))))

(define char-ci<? #void)
(define char-ci<=? #void)
(define char-ci=? #void)
(define char-ci>? #void)
(define char-ci>=? #void)

(let ((make-char-ci-comparator
        (lambda (comparator)
          (lambda s
            (apply comparator
              (map (lambda (ch)
                     (char->integer
                       (char-downcase ch)))
                s))))))
  (set! char-ci<? (make-char-ci-comparator <))
  (set! char-ci<=? (make-char-ci-comparator <=))
  (set! char-ci=? (make-char-ci-comparator =))
  (set! char-ci>? (make-char-ci-comparator >))
  (set! char-ci>=? (make-char-ci-comparator >=)))

(define string-downcase #void)
(define string-upcase #void)

(let ((make-string-case-converter
        (lambda (char-case-converter)
          (lambda (str)
            (list->string
              (map char-case-converter
                (string->list str)))))))
  (set! string-downcase (make-string-case-converter char-downcase))
  (set! string-upcase (make-string-case-converter char-upcase)))

(define string<? #void)
(define string<=? #void)
(define string=? #void)
(define string>=? #void)
(define string>? #void)
(define string-ci<? #void)
(define string-ci<=? #void)
(define string-ci=? #void)
(define string-ci>=? #void)
(define string-ci>? #void)



; (define make-string-run
;        (lambda (i str1 len1 str2 len2)
;               (or (and (= i len1) (< len1 len2))
;               (and
;                      (< i len1)
;                      (or (char<?
;                      (string-ref str1 i)
;                      (string-ref str2 i))
;                      (and (char=?
;                             (string-ref str1 i)
;                             (string-ref str2 i))
;                             (make-string-run (+ i 1) str1 len1 str2 len2)))))))
       

; (define make-string-run-bin
;        (lambda (str strs)
;        (or (null? strs)
;               (and (binary-string<? str (car strs)) (make-string-run-bin (car strs) (cdr strs))))))

; (let ((make-string<?
;         (lambda (char<? char=?)
;           (letrec (run make-string-run)
;             (let ((binary-string<?
;                     (lambda (str1 str2)
;                       (let ((len1 (string-length str1))
;                             (len2 (string-length str2)))
;                         (if (<= len1 len2)
;                             (run 0 str1 len1 str2 len2)
;                             (run 0 str2 len2 str1 len1))))))
;               (letrec ((run make-string-run-bin))
;                 (lambda (str . strs)
;                   (run str strs))))))))
;   (set! string<? (make-string<? char<? char=?))
;   (set! string-ci<? (make-string<? char-ci<? char-ci=?))
;   (set! string>? (make-string<? char>? char=?))
;   (set! string-ci>? (make-string<? char-ci>? char-ci=?)))


; (define make-string-run1 (lambda (i str1 len1 str2 len2)
;                        (or (= i len1)
;                            (char<?
;                              (string-ref str1 i)
;                              (string-ref str2 i))
;                            (and (< i len1)
;                                 (char=?
;                                   (string-ref str1 i)
;                                   (string-ref str2 i))
;                                 (run (+ i 1) str1 len1 str2 len2)))))

; (define make-string-run-bin1 (lambda (str strs)
;                            (or (null? strs)
;                                (and (binary-string<=? str (car strs)) (run (car strs) (cdr strs))))))

; (let ((make-string<=?
;         (lambda (char<? char=?)
;           (let ((run make-string-run1))
;             (let ((binary-string<=?
;                     (lambda (str1 str2)
;                       (let ((len1 (string-length str1))
;                             (len2 (string-length str2)))
;                         (if (<= len1 len2)
;                             (run 0 str1 len1 str2 len2)
;                             (run 0 str2 len2 str1 len1))))))
;               (let ((run make-string-run-bin1))
;                 (lambda (str . strs)
;                   (run str strs))))))))
;   (set! string<=? (make-string<=? char<? char=?))
;   (set! string-ci<=? (make-string<=? char<? char=?))
;   (set! string>=? (make-string<=? char>? char=?))
;   (set! string-ci>=? (make-string<=? char-ci>? char-ci=?)))

; (define make-string-run2 (lambda (i str1 str2 len)
;                        (or (= i len)
;                            (and (< i len)
;                                 (char=?
;                                   (string-ref str1 i)
;                                   (string-ref str2 i))
;                                 (run (+ i 1) str1 str2 len)))))

; (define make-string-run-bin2 (lambda (str strs)
;                            (or (null? strs)
;                                (and (binary-string=? str (car strs)) (run (car strs) (cdr strs))))))

; (let ((make-string=?
;         (lambda (char=?)
;           (let ((run make-string-run2))
;             (let ((binary-string=?
;                     (lambda (str1 str2)
;                       (let ((len1 (string-length str1))
;                             (len2 (string-length str2)))
;                         (and (= len1 len2)
;                              (run 0 str1 str2 len1))))))
;               (let ((run make-string-run-bin2))
;                 (lambda (str . strs)
;                   (run str strs))))))))
;   (set! string=? (make-string=? char=?))
;   (set! string-ci=? (make-string=? char-ci=?)))

; (define length
;   (lambda (s)
;     (if (null? s)
;         0
;         (+ 1 (length (cdr s))))))




; (define positive?
;   (lambda (x)
;     (< 0 x)))

; (define negative? (lambda (x) (< x 0)))

; (define even? (lambda (n) (zero? (remainder n 2))))

; (define odd? (lambda (n) (not (even? n))))

; (define abs (lambda (x) (if (negative? x) (- x) x)))


; (define equal?
;   (lambda (e1 e2)
;     (cond ((and (pair? e1) (pair? e2))
;            (and (equal? (car e1) (car e2))
;                 (equal? (cdr e1) (cdr e2))))
;           ((and (vector? e1) (vector? e2)
;                 (= (vector-length e1) (vector-length e2)))
;            (equal? (vector->list e1) (vector->list e2)))
;           ((and (string? e1) (string? e2)
;                 (= (string-length e1) (string-length e2)))
;            (string=? e1 e2))
;           (else (eq? e1 e2)))))

; (define assoc
;   (lambda (a s)
;     (cond ((null? s) #f)
;           ((eq? (caar s) a) (car s))
;           (else (assoc a (cdr s))))))
