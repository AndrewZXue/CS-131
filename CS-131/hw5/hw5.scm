#lang racket


(define (null-ld? obj)
	(cond ((null? obj) #f)
		((not (pair? obj)) #f)
		((not (eq? (car obj) (cdr obj))) #f )
		(else #t)))

(define (ld? obj)
	(cond ((null-ld? obj) #t)
		((null? obj) #f)
		((not (pair? obj)) #f)
		(else (cond ((not (pair? (car obj))) #f)
					(else (ld? (cons (cdr(car obj)) (cdr obj))))))))

(define (cons-ld obj listdiff)
	(cons 
		(cons obj (car listdiff)) 
		(cdr listdiff)))


(define (car-ld listdiff)
	(cond ((null? listdiff) "error")
		((not (ld? listdiff)) "error")
		((null-ld? listdiff) "error")
		(else (car(car listdiff)))))

(define (cdr-ld listdiff)
	(cond ((null? listdiff) "error")
		((not (ld? listdiff)) "error")
		((null-ld? listdiff) "error")
		(else (cons (cdr(car listdiff)) (cdr listdiff)))))

(define (ld obj . argv)
	(cons 
		(cons obj argv) 
		'()))

(define (length-ld listdiff)
	(cond ((not(ld? listdiff)) "error")
		(else (let length-ld-rec ((x (car listdiff)))
			(cond ((eq? (cdr listdiff) x) 0)
				(else (+ 1 (length-ld-rec (cdr x)))))))))

(define (ld-tail listdiff k)
	(cond ((not (ld? listdiff)) "error")
		((< k 0) "error")
		((> k (length-ld listdiff)) "error")
		((eq? k 0) listdiff)
		(else (ld-tail (cons (cdr (car listdiff)) (cdr listdiff)) (- k 1) ))))

(define (list->ld list)
	(cons list '()))

(define (ld->list listdiff)
	(cond ((not (ld? listdiff)) "error")
		((eq? (car listdiff) (cdr listdiff)) null)
		(else (cons 
				(car-ld listdiff)
				(ld->list (cdr-ld listdiff))))))

(define (append-ld listdiff . argv)
		(if (null? argv) listdiff
      		(apply append-ld (cons (append (take (car listdiff) (length-ld listdiff)) (car (car argv))) 
      			(cdr (car argv))) 
           	(cdr argv))))


(define map-ld 
	(lambda (proc listdiff . argv)
	(if (null? argv)
		(let map-ld-one ((ls listdiff))
			(if (null-ld? ls)
				'()
				(cons (proc (car-ld ls))
					(map-ld-one (cdr-ld ls)))))
		(let map-ld-more ((ls listdiff) (args argv))
			(if (null-ld? ls)
				'()
				(cons (apply proc (car-ld ls) (map car-ld args))
					(map-ld-more (cdr-ld ls)
						(map cdr-ld args))))))))



(define (translate expr)
	(cond ((eq? expr 'map) 'map-ld)
		((eq? expr 'list) 'ld)
		((eq? expr 'null?) 'null-ld?)
		((eq? expr 'list?) 'ld?)
		((eq? expr 'length) 'length-ld)
		((eq? expr 'append) 'append-ld)
		((eq? expr 'list-tail) 'ld-tail)
		((eq? expr 'car) 'car-ld)
		((eq? expr 'cdr) 'cdr-ld)
		((eq? expr 'cons) 'cons-ld)
		(else expr)
	))

(define (expr2ld expr)
	(cond ((null? expr) null)
		((not (list? (car expr))) (cons (translate (car expr)) (expr2ld (cdr expr))))
		(else (cons (expr2ld (car expr)) (expr2ld (cdr expr)))) 
	)
)

#| (define ils (append '(a e i o u) 'y))
(define d1 (cons ils (cdr (cdr ils))))
(define d2 (cons ils ils))
(define d3 (cons ils (append '(a e i o u) 'y)))
(define d4 (cons '() ils))
(define d5 0)
(define d6 (ld ils d1 37))
(define d7 (append-ld d1 d2 d6))

(ld? d1)                              
(ld? d2)                            
(ld? d3)                          
(ld? d4)                     
(ld? d5)                          
(ld? d6)                             
(ld? d7)                             

(null-ld? d1)                       
(null-ld? d2)                         
(null-ld? d3)                        
(null-ld? d6)                      

(car-ld d1)                        
(car-ld d2)                   
(car-ld d3)            
(car-ld d6)                    
(length-ld d1)                    
(length-ld d2)                       
(length-ld d3)                        
(length-ld d6)                      
(length-ld d7)                       

(define kv1 (cons d1 'a))
(define kv2 (cons d2 'b))
(define kv3 (cons d3 'c))
(define kv4 (cons d1 'd))
(define d8 (ld kv1 kv2 kv3 kv4))
(define d9 (ld kv3 kv4))
(eq? d8 (ld-tail d8 0))                
(equal? (ld->list (ld-tail d8 2))
        (ld->list d9))               
(null-ld? (ld-tail d8 4))             
(ld-tail d8 -1)                       
(ld-tail d8 5)                       

(eq? (car-ld d6) ils)               
(eq? (car-ld (cdr-ld d6)) d1)          
(eqv? (car-ld (cdr-ld (cdr-ld d6))) 37)
(equal? (ld->list d6)
        (list ils d1 37))           
(eq? (list-tail (car d6) 3) (cdr d6))  

(define e1 (expr2ld '(map (lambda (x) (+ x 1))
                          (list (length (list d1)) 2 4 8)
                          (append (list) (list-tail (list 1 16 32) 1)))))
(equal? e1 '(map-ld (lambda (x) (+ x 1))
                    (ld (length-ld (ld d1)) 2 4 8)
                    (append-ld (ld) (ld-tail (ld 1 16 32) 1)))) |#


#| (define ils (append '(1 2 3 4 5) 6))
(define itest (cons ils (cdr (cdr ils))))
(define jls (append '(7 8 9 10 11) 12))
(define jtest (cons jls (cdr (cdr jls))))
(define kls (append '(2 3 4 10 11) 12))
(define ktest (cons kls (cdr (cdr kls))))

(ld->list itest)
(ld->list jtest)

(map-ld * itest jtest ktest)
 |#


#| 
(define (pprint listdiff . argv) (map car-ld argv))

(pprint itest jtest ktest) |#
