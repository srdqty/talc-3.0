(define (eqv? x y)  (eq? x y)) ;; not structural equality, but should be more than eq?

;; rational arithmetic, floating pt, bignums
(define (exact? x) #f)
(define (remainder x y) 0)
(define (inexact? x) #t)

;; vectors
(define (vector? x) #f)
(define (vector-length v) 0)
(define (vector-ref v i) #f)

;;
;; All of the string functions
(define (string=? x y) #f)

;; This is actually a serious flaw in our implementation.
;; All primitives should be identifiers, so that they may be
;; passed as arguments (and changed!). As a workaround we
;; can eta-expand for now
(define (%eq? x y) (eq? x y))

; Vararg functions (all args are packed into a list when called)
; also display
;(define (newline . p)
;	(if (null? p) (display "#0a")
;		(display "#0a" (car p))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; std library - mostly stolen from LispMe
;;
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (abs n)
	(if (>= n 0) n (* (- 0 1) n)))

(define (append x y)
	(if (null? x) y
	(cons (car x)
		 (append (cdr x) y))))

(define (%assoc test)
	(lambda (x ls)
	(letrec ((ahelp (lambda (l)
			  (cond
			   ((null? l) #f)
			   ((test x (caar l)) (car l))
			   (else (ahelp (cdr l)))))))
	  (ahelp ls))))

(define (assoc x ls)
  ((%assoc equal?) x ls))

(define (assq x ls)
  ((%assoc %eq?) x ls))

(define (assv x ls)
	((%assoc eqv?) x ls))

(define (equal? a b)
  (cond ((eqv? a b) #t)
	((and (string? a) (string? b))
	    (string=? a b))
	((and (pair? a) (pair? b))
	    (and (equal? (car a) (car b))
		    (equal? (cdr a) (cdr b))))
	((and (vector? a) (vector? b))
	    (%vec-equal? a b))
	(else #f)))

(define (%vec-equal? a b)
  (letrec ((help (lambda (k)
	(if (eq? k -1) #t
	(and (equal? (vector-ref a k)
			(vector-ref b k))
		(help (- k 1)))))))
  (let ((l (vector-length a)))
	(and (eq? l (vector-length b))
	  (help (- l 1))))))

(define (even? n) (= (remainder n 2) 0))

(define (for-each f l)
  (if (null? l)
      #f
      (begin (f (car l))
	     (for-each f (cdr l)))))

(define (length l)
	(letrec ((iter (lambda (l n)
		    (if (null? l) n
		      (iter (cdr l) (+ 1 n))))))
	(iter l 0)))

(define (list-ref ls n)
	(if (eq? n 0) (car ls)
		(list-ref (cdr ls) (- n 1))))

(define (map f l)
	(if (null? l) '()
	(cons (f (car l))
		 (map f (cdr l)))))

(define (max a b)
	(if (<= a b) b a))

(define (%member test)
	(lambda (x ls)
	(letrec
	((mhelp (lambda (l)
	    (cond  ((null? l) #f)
		((test x (car l)) l)
		(else (mhelp (cdr l)))))))
 	(mhelp ls))))

(define (member x ls)
	((%member equal?) x ls))

(define (memq x ls)
	((%member %eq?) x ls))

(define (memv x ls)
	((%member eqv?) x ls))

(define (min a b)
	(if (<= a b) a b))

(define (modulo a b)
	(let ((r (remainder a b)))
	(if (>= (* r b) 0) r
		(+ r b))))

(define (negative? x) (< x 0))

(define (odd? n) (= (remainder n 2) 1))

(define (port? p) (or (input-port? p) (output-port? p)))

(define (positive? x) (> x 0))

(define (reverse l)
	(letrec
	((rev (lambda (a b)
		 (if (null? a) b
		(rev (cdr a)
			 (cons (car a) b))))))
 	(rev l '())))

(define (zero? x) (eqv? x 0))
