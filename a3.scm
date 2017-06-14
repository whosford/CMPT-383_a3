(define (my-last lst)
	(cond
		((null? lst)
			(error "empty list"))
		((null? (cdr lst)) 
			(car lst))
		(else 
			(my-last (cdr lst)))
	)
)

(define (snoc x lst)
	(cond
		((null? lst)
			(cons x '()))
		((null? x)
			lst)
		(else
			(cons (car lst) (snoc x (cdr lst))))
	)
)

(define (range n)
	(cond 
		((<= n 0)
			'())
		((= 0 (- n 1))
			(cons 0 '()))
		(else 
			(snoc (- n 1) (range (- n 1)))
		)
	)	
)

(define (deep-sum lst)
	(cond
		((null? lst)
			0)
		((list? (car lst))
			(+ (deep-sum (car lst)) (deep-sum (cdr lst))))
		((number? (car lst))
			(+ (car lst) (deep-sum (cdr lst))))
		(else 
			(deep-sum (cdr lst)))
	)
)

(define divides?
	(lambda (a b)
		(= (remainder a b) 0)
	)
)

(define prime?
  	(lambda (n)
	    (cond 
	    	((or (= n 1) (= n 0)) 
	    		#f)
	      	((= n 2) 
	      		#t)
	      	((even? n) 
	      		#f)
	      	(else 
	      		(let loop ((i 3))
	          		(cond 
	          			((> (* i i) n) 
	          				#t)
	            		((divides? n i) 
	            			#f)
	            		(else 
	            			(loop (+ i 2)))
	            	)
	          	)
	      	)
      	)
	)
)

(define (count-primes n)
	(cond 
		((<= n 1)
			0)
		(else 
			(+ (count-primes (- n 1)) (if (prime? n) 1 0)))
	)
)

(define (is-bit? x)
	(and (number? x) (or (= x 1) (= x 0)))
)

(define (is-bit-seq? lst)
	(cond 
		((null? lst)
			#t)
		((is-bit? (car lst))
			(is-bit-seq? (cdr lst)))
		(else 
			#f)
	)
)

(define (make-list n)
	(cond 
		((= n 0) '())
		(else
			(cons 0 (make-list (- n 1)))
		)
	)
)

(define (change-element lst x pos)
	(cond 
		((or (null? lst) (>= pos (length lst))) lst)
		(else 
			(if (= pos 0) 
				(cons x (cdr lst))
				(cons (car lst) (change-element (cdr lst) x (- pos 1)))
			)
		)
	)
)

(define (bit-list n lst)
	(cond 
		((<= n 0)
			(cons lst '()))
		(else
			(append
				(bit-list (- n 1) (change-element lst 0 (- n 1)))
				(bit-list (- n 1) (change-element lst 1 (- n 1)))
			)
		)
	)
)

(define (all-bit-seqs n)
	(cond 
		((<= n 0)
			'())
		(else
			(bit-list n (make-list n))
		)
	)
)