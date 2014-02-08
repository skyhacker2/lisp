(define (mult6 x)
  (* x 4))
(define (square x)
  (* x x))
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

;lat?
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l))(lat? (cdr l)))
     (else #f))))

;member?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

;rember
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) a) (cdr lat))
	    (else (cons (car lat)
			(rember a (cdr lat)))))))))

;firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
		 (firsts (cdr l)))))))
;insertR
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (eq? (car lat) old)
	   (cons old (new (cdr lat)))
	   (else (cons (car lat) (insertR new old (cdr lat))))))))

;insertL
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (eq? (car lat) old)
	   (cons new lat)
	   (else (cons (car lat) (insertL new old (cdr lat))))))))

;subst
(define subst
  (lambda (new old lat)
    (cond
    ((null? lat) (quote()))
    (else (eq? (car lat) old)
	  (cons new (cdr lat))
	  (else (cons (car lat) (subst new old (cdr lat))))))))

;subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
	    (or (eq? (car lat) o1) (eq? (car lat) o2)
		(cons new (cdr lat)))
	    (else (cons (car lat) (subst new o1 o2 (cdr lat)))))))))

;multirember
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    (eq? (car lat) a)
	    (multirember a (cdr lat))
	    (else (cons (car lat)
			(multirember a (cdr lat)))))))))
;multisubst
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new (multisubst new old (cdr lat))))
	    (else (cons (car lat) (multisubst new old (cdr lat)))))))))

;rember*
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(rember* a (cdr l)))
       (else (cons (car l)
		   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
		 (rember * a (cdr l)))))))
      
 ;insertR*
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old (cons new (insertR* new old (cdr l)))))
       (else
	(cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;occur*
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l))
	(add1 a (occur* a (cdr l))))
       (else
	(occur* a (cdr l))))
      (else
       (+ (occur* (car l) (occur* cdr l))))))))

;subst*
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (subst* new old (cdr l))))
       (else
	(subst* new old (cdr l)))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;insertL* 
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (cons old (insertL* new old (cdr l)))))
       (else
	(insertL* new old (cdr l)))))
     (else
      (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
	
      
	