; append function named as d
(define (d lis1 lis2)
  (cond ((null? lis1) lis2)
        (else (cons (car lis1)
                    (d (cdr lis1) lis2)))))

; loopy and looping is to find out the last element in the list
(define (loopy liss current idx)
	(cond	((equal? liss '()) idx)
		((equal? (car liss) 1) (loopy (cdr liss) (+ 1 current) (+ 1 current)))
		(#t (loopy (cdr liss) (+ 1 current) idx))))

(define (looping liss)
	(loopy liss 0 0))

; lisone is to create an all 1s list
(define (lisone liss idx)
	(cond	((equal? idx 0) (cons 1 (cdr liss)))
		(#t (cons (car liss) (lisone (cdr liss) (- idx 1))))))

; nega functions are to take lists with 0 and 1
(define (nega ret liss length idx)
	(cond	((equal? (+ 1 length) idx) ret)
		(#t (nega (cons (lisone liss length) ret) liss (- length 1) idx))))

(define (negate liss length)
	(nega '() liss (- length 1) (looping liss)))

(define (negation liss length)
	(cond	((equal? liss '()) '())
		((number? (car liss)) (negate liss length))
		(#t (d (negation (car liss) length) (negation (cdr liss) length)))))

; wrap and wrapper is to get the list size
(define (wrap liss length)
	(cond	((equal? liss '()) length)
		(#t (wrap (cdr liss) (+ 1 length)))))

(define (wrapper liss)
	(wrap liss 0))

; create mappings for the list
(define (mapping liss length num)
	(cond   ((equal? num length) (d liss (recusion length num)))
		(#t (mapping (d liss (recusion length num)) length (+ 1 num)))))

; recusions funcions call several times the negation function
(define (recus liss length num)
	(cond   ((equal? 0 num) liss)
		(#t (recus (negation liss length) length (- num 1)))))

(define (recusion length num)
	(recus (list (zolist length)) length num))

(define (pmap length)
	(mapping '() length 0))

;zlist and zolist are to create a list with all 0s
(define (zlist liss num)
	(cond	((equal? 0 num) liss)
		(#t (zlist (cons 0 liss) (- num 1)))))

(define (zolist num)
	(zlist '() num))

(define (sublis lis amap)
	(cond	((equal? amap '()) '())
		((equal? (car amap) 0) (sublis (cdr lis) (cdr amap)))
		(#t (cons (car lis) (sublis (cdr lis) (cdr amap))))))

(define (powerlis amap lis)
	(cond	((equal? amap '()) '())
		(#t (cons (sublis lis (car amap)) (powerlis (cdr amap) lis)))))

; the Power function to get power sets
(define (POWER lis)
	(powerlis (pmap (wrapper lis)) lis))

;examples to test
(POWER '())
(POWER '(1))
(POWER '(1 2))
(POWER '(1 2 3))
(POWER '(1 2 3 4))
