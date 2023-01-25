;;; init.scm
;;; Initial definitions that should be available for the compiler

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
(define list*-run (lambda (a s)
               (if (null? s)
                   a
                   (cons a
                     (run (car s) (cdr s))))))

(define list*
  (let ((run list*-run))
    (lambda (a . s)
      (run a s))))

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
(define list-mul
  (lambda (s)
    (if (null? s) 1
        (bin* (car s) (list-mul (cdr s)))
    )
  )
)

(define * 
  (lambda s
    (list-mul s)
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
        (bin/ (car s) (list-mul (cdr s)))
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


; (let* ((exit
;          (lambda ()
;            (error 'generic-comparator
;              "all the arguments must be numbers")))
;        (make-bin-comparator
;          (lambda (comparator-qq comparator-rr)
;            (lambda (a b)
;              (cond ((rational? a)
;                     (cond ((rational? b) (comparator-qq a b))
;                           ((real? b)
;                            (comparator-rr (rational->real a) b))
;                           (else (exit))))
;                    ((real? a)
;                     (cond ((rational? b)
;                            (comparator-rr a (rational->real b)))
;                           ((real? b) (comparator-rr a b))
;                           (else (exit))))))))
;        (bin<? (make-bin-comparator __bin-less-than-qq __bin-less-than-rr))
;        (bin=? (make-bin-comparator __bin-equal-qq __bin-equal-rr))
;        (bin>=? (lambda (a b) (not (bin<? a b))))
;        (bin>? (lambda (a b) (bin<? b a)))
;        (bin<=? (lambda (a b) (not (bin>? a b)))))
;   (let ((make-run
;           (lambda (bin-ordering)
;             (letrec ((run
;                        (lambda (a s)
;                          (or (null? s)
;                              (and (bin-ordering a (car s))
;                                   (run (car s) (cdr s)))))))
;               (lambda (a . s) (run a s))))))
;     (set! < (make-run bin<?))
;     (set! <= (make-run bin<=?))
;     (set! > (make-run bin>?))
;     (set! >= (make-run bin>=?))
    ;;(set! = (make-run bin=?))))


(define list->size
  (lambda (l) 
    (if (null? l) 0
      (+ 1 (list-size (cdr l)))
    )
  )
)


(define make-list-internal
  (lambda (n init-char) 
    (if (zero? n) '()
      (cons init-char (make-list-internal (- n 1) init-char))
    )
  )
)


(define make-list
  (lambda (n . chs)
    (if (null? chs) (make-list-internal n 0)
        (make-list-internal n (car chs))
    )
  )
)

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



(define make-string-run
       (lambda (i str1 len1 str2 len2)
              (or (and (= i len1) (< len1 len2))
              (and
                     (< i len1)
                     (or (char<?
                     (string-ref str1 i)
                     (string-ref str2 i))
                     (and (char=?
                            (string-ref str1 i)
                            (string-ref str2 i))
                            (make-string-run (+ i 1) str1 len1 str2 len2)))))))
       

(define make-string-run-bin
       (lambda (str strs)
       (or (null? strs)
              (and (binary-string<? str (car strs)) (make-string-run-bin (car strs) (cdr strs))))))

(let ((make-string<?
        (lambda (char<? char=?)
          (let ((run make-string-run))
            (let ((binary-string<?
                    (lambda (str1 str2)
                      (let ((len1 (string-length str1))
                            (len2 (string-length str2)))
                        (if (<= len1 len2)
                            (run 0 str1 len1 str2 len2)
                            (run 0 str2 len2 str1 len1))))))
              (let ((run make-string-run-bin))
                (lambda (str . strs)
                  (run str strs))))))))
  (set! string<? (make-string<? char<? char=?))
  (set! string-ci<? (make-string<? char-ci<? char-ci=?))
  (set! string>? (make-string<? char>? char=?))
  (set! string-ci>? (make-string<? char-ci>? char-ci=?)))


(define make-string-run1 (lambda (i str1 len1 str2 len2)
                       (or (= i len1)
                           (char<?
                             (string-ref str1 i)
                             (string-ref str2 i))
                           (and (< i len1)
                                (char=?
                                  (string-ref str1 i)
                                  (string-ref str2 i))
                                (run (+ i 1) str1 len1 str2 len2)))))

(define make-string-run-bin1 (lambda (str strs)
                           (or (null? strs)
                               (and (binary-string<=? str (car strs)) (run (car strs) (cdr strs))))))

(let ((make-string<=?
        (lambda (char<? char=?)
          (let ((run make-string-run1))
            (let ((binary-string<=?
                    (lambda (str1 str2)
                      (let ((len1 (string-length str1))
                            (len2 (string-length str2)))
                        (if (<= len1 len2)
                            (run 0 str1 len1 str2 len2)
                            (run 0 str2 len2 str1 len1))))))
              (let ((run make-string-run-bin1))
                (lambda (str . strs)
                  (run str strs))))))))
  (set! string<=? (make-string<=? char<? char=?))
  (set! string-ci<=? (make-string<=? char<? char=?))
  (set! string>=? (make-string<=? char>? char=?))
  (set! string-ci>=? (make-string<=? char-ci>? char-ci=?)))

(define make-string-run2 (lambda (i str1 str2 len)
                       (or (= i len)
                           (and (< i len)
                                (char=?
                                  (string-ref str1 i)
                                  (string-ref str2 i))
                                (run (+ i 1) str1 str2 len)))))

(define make-string-run-bin2 (lambda (str strs)
                           (or (null? strs)
                               (and (binary-string=? str (car strs)) (run (car strs) (cdr strs))))))

(let ((make-string=?
        (lambda (char=?)
          (let ((run make-string-run2))
            (let ((binary-string=?
                    (lambda (str1 str2)
                      (let ((len1 (string-length str1))
                            (len2 (string-length str2)))
                        (and (= len1 len2)
                             (run 0 str1 str2 len1))))))
              (let ((run make-string-run-bin2))
                (lambda (str . strs)
                  (run str strs))))))))
  (set! string=? (make-string=? char=?))
  (set! string-ci=? (make-string=? char-ci=?)))

(define length
  (lambda (s)
    (if (null? s)
        0
        (+ 1 (length (cdr s))))))

(define list?
  (lambda (e)
    (or (null? e)
        (and (pair? e)
             (list? (cdr e))))))

(define make-vector
  (let ((asm-make-vector make-vector))
    (lambda (n . xs)
      (let ((x
              (cond ((null? xs) #void)
                    ((and (pair? xs)
                          (null? (cdr xs)))
                     (car xs))
                    (else (error 'make-vector
                            "Usage: (make-vector size ?optional-default)")))))
        (asm-make-vector n x)))))

(define make-string
  (let ((asm-make-string make-string))
    (lambda (n . chs)
      (let ((ch
              (cond ((null? chs) 'nul)  ; TODO : cant use #\nul
                    ((and (pair? chs)
                          (null? (cdr chs)))
                     (car chs))
                    (else (error 'make-string
                            "Usage: (make-string size ?optional-default)")))))
        (asm-make-string n ch)))))

(define list->vector-run (lambda (s i)
               (if (null? s)
                   (make-vector i #void)
                   (let ((v (run (cdr s) (+ i 1))))
                     (vector-set! v i (car s))
                     v))))

(define list->vector
  (let ((run list->vector-run))
    (lambda (s)
      (run s 0))))

(define list->string-run (lambda (s i)
               (if (null? s)
                   (make-string i 'nul)  ; TODO : cant use #\nul
                   (let ((str (run (cdr s) (+ i 1))))
                     (string-set! str i (car s))
                     str))))

(define list->string
  (let ((run make->string-run))
    (lambda (s)
      (run s 0))))
      
(define vector (lambda s (list->vector s)))

(define string->list-run (lambda (str i n)
               (if (< i n)
                   (cons (string-ref str i)
                     (run str (+ i 1) n))
                   '())))

(define string->list
  (let ((run string->list-run))
    (lambda (str)
      (run str 0 (string-length str)))))

(define vector->list-run (lambda (v i n)
               (if (< i n)
                   (cons (vector-ref v i)
                     (run v (+ i 1) n))
                   '())))

(define vector->list
  (let ((run vector->list-run))
    (lambda (v)
      (run v 0 (vector-length v)))))

(define random (lambda (n) (remainder (trng) n)))

(define positive?
  (lambda (x)
    (< 0 x)))

(define negative? (lambda (x) (< x 0)))

(define even? (lambda (n) (zero? (remainder n 2))))

(define odd? (lambda (n) (not (even? n))))

(define abs (lambda (x) (if (negative? x) (- x) x)))


(define equal?
  (lambda (e1 e2)
    (cond ((and (pair? e1) (pair? e2))
           (and (equal? (car e1) (car e2))
                (equal? (cdr e1) (cdr e2))))
          ((and (vector? e1) (vector? e2)
                (= (vector-length e1) (vector-length e2)))
           (equal? (vector->list e1) (vector->list e2)))
          ((and (string? e1) (string? e2)
                (= (string-length e1) (string-length e2)))
           (string=? e1 e2))
          (else (eq? e1 e2)))))

(define assoc
  (lambda (a s)
    (cond ((null? s) #f)
          ((eq? (caar s) a) (car s))
          (else (assoc a (cdr s))))))

;;; end-of-input
