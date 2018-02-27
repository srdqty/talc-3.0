(define revappend
  (lambda (x y) (if (null? x) y (revappend (cdr x) (cons (car x) y)))))

(define reverse (lambda (x) (revappend x '())))

(define map
  (lambda (f x) (if (null? x) '() (cons (f (car x)) (map f (cdr x))))))

(define fold
  (lambda (f l a) (if (null? l) a (fold f (cdr l) (f (car l) a)))))

(define print-int
  (lambda (x)
     (letrec ((print-digit (lambda (x) (write-char x)))
	     (digits (lambda (x cs)
		       (if (= x 0) cs
			   (let ((y (/ x 10))
				 (d (- x (* y 10)))
				 (c (integer->char
				     (+ d
					(char->integer (string-ref "0" 0))))))
			     (digits y (cons c cs)))))))
      (cond ((= x 0) (write-char-const "0"))
	    ((< x 0) (begin (write "-") (print-int (- 0 x))))
	    (else (map print-digit (digits x '())))))))

(define curry
  (lambda (f)
    (lambda (x)
      (lambda (y) (f x y)))))

(define uncurry
  (lambda (f)
    (lambda (x y) ((f x) y))))

(define fact
  (lambda (n)
    (if (= n 1) 1 (* n (fact (- n 1))))))

;;  hack since parsers don't read in characters at the moment
(define write-char-const (lambda (x) (write-char (string-ref x 0))))
	
(define print-value
  (lambda (x)
    (letrec ((print-char (lambda (x)
			   (write-char-const "'")
			   (write-char x)
			   (write-char-const "'")))
	     (print-string (lambda (x)
			     (write-char-const "\"")
			     (write x)
			     (write-char-const "\"")))
	     (print-pair (lambda (x)
			   (write-char-const "(")
			   (print-value (car x))
			   (write-char-const ".")
			   (print-value (cdr x))
			   (write-char-const ")"))))
      (cond ((null? x) (write "'()"))
	    ((integer? x) (print-int x))
	    ((pair? x) (print-pair x))
	    ((char? x) (print-char x))
	    ((string? x) (print-string x))
	    ((input-port? x) (write "#indesc"))
	    ((output-port? x) (write "#outdesc"))
	    ((procedure? x) (write "#fn"))
	    (else (write "#t"))))))

(define print-val (lambda (x) (print-value x) (write-char (integer->char 10))))

(define split
  (lambda (l1 l2 l3)
    (if (null? l1) (cons l2 l3)
	(split (cdr l1) l3 (cons (car l1) l2)))))

(define merge
  (lambda (l1 l2 l3)
    (cond ((null? l1) (revappend l2 l3))
	  ((null? l2) (revappend l1 l3))
	  (else
	   (let ((x1 (car l1))
		 (x2 (car l2)))
	     (if (< x1 x2) 
		 (merge (cdr l1) l2 (cons x1 l3))
		 (merge l1 (cdr l2) (cons x2 l3))))))))

(define mergesort
  (lambda (l)
    (cond ((null? l) l)
	  ((null? (cdr l)) l)
	  (else
	   (let ((p (split l '() '())))
	     (reverse (merge (mergesort (car p)) 
			     (mergesort (cdr p)) '())))))))

(define insertionsort
  (lambda (cmp l)
    (letrec ((insert (lambda (i x)
		       (cond ((null? x) (cons i '()))
			     ((cmp i (car x)) (cons i x))
			     (else (cons (car x) (insert i (cdr x))))))))
      (fold insert l '()))))
			   

(define generate 
  (lambda (i l) (if (= i 0) l (generate (- i 1) (cons i l)))))

(define up   (generate 100 '()))
(define down (insertionsort (lambda (a b) (> a b)) up))
(define up2  (mergesort     down))
(define up3  (insertionsort (lambda (a b) (< a b)) up2))

(define output 
  (begin 
    (write "Factorial of 5: ")
    (write (fact 5))
    (write "Lots of sorting using write keyword (C implementation): ")
    (write up) 
    (write down) 
    (write up2)
    (write up3)
    (write "Same sorting using write-char and scheme code: ")
    (print-val up) 
    (print-val down) 
    (print-val up2)
    (print-val up3)))

		
		   
