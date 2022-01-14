(define recur (lambda (f x ds) (f x (car ds) (cdr ds))))
(define pair-head
  (lambda (e dds)
    (cond   ((null? e) #t)
            ((eq? (car e) (car dds))(pair-head (cdr e) (cdr dds)))
            (else #f))))

(define glide-search
  (lambda (e1 e2 ddds)
    (cond   ((null? ddds) null)
            ((pair-head e1 ddds) (cons e2 (recur glide-search e1 ddds)))
            (else (recur glide-search e1 ddds)))))

(define before-seq
    (lambda (xs ys)
      (cond ((or (null? xs) (null? ys)) ys)
            (else (remove null (glide-search xs null ys))))))

(define ddx
  (λ (e)
  (if (number? e)
	0
	(if (equal? e 'x)
    1
	  (if (equal? (car e) '+)
		  (list '+ (ddx (cadr e)) (ddx (caddr e)))
		  (if (equal? (car e) '*)
		    (let* ([x (cadr e)] [y (caddr e)] [m (ddx y)] [n (ddx x)])
		    (cond 
          ((and (and (number? m) (= m 0)) (and (number? m) (= n 0))) 0)
          ((and (number? m) (= m 0))(list '+  0 (list '* n y)))
          ((and (number? m) (= n 0))(list '+  (list '* x m 0)))
          (else (list '+  (list '* x m)
			              (list '* n y)))))
		    (error 'oops)))))))

(define c-ify2 (λ (f) (λ (c x y) (c (f x y)))))
(define c= (c-ify2 =))
(define c- (c-ify2 -))
(define c+ (c-ify2 +))
(define cf
  (λ (c n)
    (c= (λ (nez) 
    (if nez
        (c 1)
        (c= (λ (nezz)
                (if nezz
                    (c 1)
                    (c+ c
                    (c- (λ (nm1) (cf + nm1)) n 1)
                    (c- (λ (nm1) (cf + nm1)) n 2))))
        n 0)))
    n 1)))

(define expand-and
  (lambda (e)
    (cond ((and (eq? (length e) 1) (eq? (car e) 'and)) '#t)
          ((and (eq? (length e) 2) (eq? (car e) 'and)) (cadr e))
          ((and (> (length e) 2) (eq? (car e) 'and)) (expand-and (cdr e)))
          (else (list 'if (car e) (expand-and (cons 'and (cdr e))) '#f)))))

(define expand-or
  (lambda (e)
    (cond ((and (eq? (length e) 1) (eq? (car e) 'or)) '#f)
          ((and (eq? (length e) 2) (eq? (car e) 'or)) (cadr e))
          ((and (> (length e) 2) (eq? (car e) 'or)) (cons 'cond (expand-or (cdr e))))
          (else (cons (list (car e) '=> '(λ (x) x))
                      (if (eq? (length (cdr e)) 1)
                        (list (list 'else (cadr e)))
                        (expand-or (cdr e))))))))

(define grovel-add
  (lambda (f s)
  (if (null? s)
      0
      (let  ([x (car s)] [ds (cdr s)])
            (cond ((list? x)(+ (grovel-add f x) (grovel-add f ds)))
                  ((number? x)(if (f x) (+ x (grovel-add f ds)) (+ 0 (grovel-add f ds))))
                  (else (+ 0 (grovel-add f ds))))))))

(define (test-foo f c)
  (let*
    ([i (car c)]
     [eo (cadr c)]
     [ao (apply f i)])
    (if (equal? eo ao)
      (list 'success: 'test 'cases 'passed)
      (list 'failure 'for 'inputs i 'expected eo 'got ao))))

(define (test-before-seq)
  (list
    'tbefore-seq
    (test-foo
      before-seq 
      '(((a b) (x y z a b 1 2 3 4 a b c d a a b)) (z 4 a)))
    (test-foo
      before-seq 
      '(((a b) (a b c d)) ()))
    (test-foo
      before-seq 
      '((() (j k l m n)) (j k l m n)))
    (test-foo
      before-seq 
      '(((t) (a b t u v t t)) (b v t)))))

(define (test-ddx)
  (list
    'ddx
    (test-foo
      ddx
      '((7) 0))
    (test-foo
      ddx 
      '((x) 1))
    (test-foo
      ddx 
      '(((+ x x)) (+ 1 1)))
    (test-foo
      ddx 
      '(((+ (* x 3) x)) (+ (+ 0 (* 1 3)) 1)))))

(define (test-cf)
  (let ([f (λ (x) x)])
  (list
    'cf
    (test-foo
      cf
      (list (list f 1) 1))
    (test-foo
      cf
      (list (list f 19) 6765))
    (test-foo
      cf
      (list (list f 27) 317811))
    (test-foo
      cf
      (list (list f 30) 1346269)))))

(define (test-expand-or)
  (list
    'expand-or
    (test-foo
      expand-or
      '(((or)) #f))
    (test-foo
      expand-or
      '(((or ONE)) ONE))
    (test-foo
      expand-or
      '(((or ONE TWO THREE)) (cond (ONE => (λ (x) x)) (TWO => (λ (x) x)) (else THREE))))
    (test-foo
      expand-or
      '(((or ONE TWO THREE FOUR FIVE)) (cond (ONE => (λ (x) x)) (TWO => (λ (x) x)) (THREE => (λ (x) x)) (FOUR => (λ (x) x)) (else FIVE))))))

(define (test-expand-and)
  (list
    'expand-and
    (test-foo
      expand-and
      '(((and)) #t))
    (test-foo
      expand-and
      '(((and ONE)) ONE))
    (test-foo
      expand-and
      '(((and ONE TWO)) (if ONE TWO #f)))
    (test-foo
      expand-and
      '(((and ONE TWO THREE FOUR)) (if ONE (if TWO (if THREE FOUR #f) #f) #f)))))

(define (test-grovel-add)
  (let ([f1 (λ (x) #t)]
        [f2 (λ (x) (< x 4))]
        [f3 (λ (x) (= (* x x) 64))]
        [f4 (λ (x) (= x 9))])
  (list
    'grovel-add
    (test-foo
      grovel-add
      (list (list f1 '(a b (5 x y (z 2)))) 7))
    (test-foo
      grovel-add
      (list (list f2 '(a b (5 x y (z 2)))) 2))
    (test-foo
      grovel-add
      (list (list f3 '(u (8 b 8 ((y t 8) 2)))) 24))
    (test-foo
      grovel-add
      (list (list f4 '(9 9 (p 20 (o (x y i) 9) (z 2)))) 27)))))