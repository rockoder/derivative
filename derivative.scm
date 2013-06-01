(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))

(define (make-subt a1 a2)
	(cond ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2)) (- a1 a2))
		  (else (list '- a1 a2))))

(define (=number? exp num)
(and (number? exp) (= exp num)))

(define (make-product a1 a2)
(cond ((or (=number? a1 0) (=number? a2 0)) 0)
	  ((=number? a1 1) a2)
	  ((=number? a2 1) a1)
	  ((and (number? a1) (number? a2)) (* a1 a2))
	  (else (list '* a1 a2))))

(define (make-divi a1 a2)
	(cond ((=number? a1 0) 0)
	  ((=number? a2 0) (error "div by 0" ))	
		  ((=number? a2 1) a1)
		  ((and (number? a1) (number? a2)) (/ a1 a2))
		  (else (list '/ a1 a2)))) 


(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (subt? x) (and (pair? x) (eq? (car x) '-)))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (divi? x) (and (pair? x) (eq? (car x) '/)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp wrt)
	(cond ((number? exp) 0)

		  ((variable? exp)
			(if (same-variable? exp wrt) 1 (list '/ 'd exp 'd wrt )))
		   
		  ((sum? exp)
			(make-sum (deriv (addend exp) wrt)
					  (deriv (augend exp) wrt)))

	  ((subt? exp)
	(make-subt (deriv (addend exp) wrt)
					  (deriv (augend exp) wrt))) 	

		  ((product? exp)
			(make-sum
					(make-product (multiplier exp)
								  (deriv (multiplicand exp) wrt))
					(make-product (deriv (multiplier exp) wrt)
								  (multiplicand exp))))	

	  ((divi? exp)
	(make-divi
		(make-subt (make-product (augend exp) (deriv (addend exp) wrt)) (make-product (addend exp) (deriv (augend exp) wrt))
				 )
		(make-product (augend exp) (augend exp))
	)
	  )			
		 
	  
		  (else
			(error "unknown expression type - DERIV" exp))))
