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
					'()
				)
			)
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
			(+ 0 (deep-sum (cdr lst))))
	)
)

(define prime?
	(lambda (n)
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

(define (left-pad lst n)
	(cond 
		((= n 0)
			lst)
		(else 
			(let loop ((i 0))
				(if (< i n)
					(cons 0 (loop (+ i 1)))
					lst
				)	
			)
		)
	)
)

(define (bit-list num size)
	(cond 
		((< num 0)
			'())
		((= num 0)
			(left-pad '(0) (- size 1)))
		(else 
			(let loop ((i num) (count 0) (lst '()))
				(cond 
					((= i 0)
						(left-pad lst (- size count)))
					((even? i)
						(loop (quotient i 2) (+ count 1) (cons 0 lst)))
					(else
						(loop (quotient i 2) (+ count 1) (cons 1 lst)))
				)
			)
		)
	)		
)

(define (all-bit-seqs n)
	(cond 
		((<= n 0)
			'())
		(else 
			(let loop ((i 0))
				(if (< i (expt 2 n))
					(cons (bit-list i n) (loop (+ i 1)))
					'()
				)
			)
		)
	)
)