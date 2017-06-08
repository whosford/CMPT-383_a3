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
		(else 
			(let loop ((m 0))
				(if (<= m n)
					(cons m (loop (+ m 1)))
					'())))
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
			(+ 0 (deep-sum (cdr lst))))
	)
)

(define (prime? n)
	(let loop ((i 2))
    	(cond 
    		((< n (* i i)) 
    			#t)
        	((zero? (modulo n i)) 
         		#f)
        	(else 
        		(loop (+ i 1)))
    	)
	)
)

(define (count-primes n)
	(let loop ((x n) (result 0))
		(cond 
			((<= x 1)
				result)
			((prime? x)
				(loop (- x 1) (+ 1 result)))
			(else 
				(loop (- x 1) result))
		)
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

(define (all-bit-seqs n)
	(cond
		((< n 1)
			'())
		(else
			(list (append (list 1) (all-bit-seqs(- n 1))) (append (list 0) (all-bit-seqs (- n 1)))))
	)
)
;(define (all-bit-seqs n)
;	(let loop ((i 0) (list))
;		(cond 
;			((< ))
;		)
;	)
;)