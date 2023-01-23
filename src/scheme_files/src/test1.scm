(define +
  (let* ((error (lambda () (error '+ "all arguments need to be numbers")))
         (bin+
           (lambda (a b)
             (cond ((rational? a)
                    (cond ((rational? b) (__bin-add-qq a b))
                          ((real? b) (__bin-add-rr (rational->real a) b))
                          (else (error))))
                   ((real? a)
                    (cond ((rational? b) (__bin-add-rr a (rational->real b)))
                          ((real? b) (__bin-add-rr a b))
                          (else (error))))
                   (else (error))))))
    (lambda (s) (fold-left bin+ 0 s))))