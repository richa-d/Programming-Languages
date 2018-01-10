;; QUESTION 1
;; Function (fromTo k n) prints integers from k to n, both inclusive
;; Base Case: If k > n, then size of the problem is 0, and return an empty list
;; Hypothesis: Assume that (fromTo (+ k 1) n) returns the list of numbers from k+1 to n, which has a problem size one smaller than the orginal problem
;; Recursive Step: (fromTo k n) = (cons k (fromTo (+ k 1) n))

> (define (fromTo k n)
    (cond ((> k n) '())
          (else (cons k (fromTo (+ k 1) n)))))

;; > (fromTo 3 8)
;; (3 4 5 6 7 8)



;; QUESTION 2
;; Function (removeMults m L) removes all multiples of m from the list L
;; Base Case: If L is empty, return an empty list
;; Hypothesis: Assume that (removeMults m (cdr L)) returns the result of removing the multiples from the remaining part of the list, which has a problem ;; size one smaller than the original problem
;; Recursive Step: (removeMults m L) = (removeMults m (cdr L)) if car L is a multiple, else (removeMults m L) = (cons (car L) (removeMults m (cdr L)))

> (define (removeMults m L)
  	(cond ((null? L) '())
        ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
        (else (cons (car L) (removeMults m (cdr L))))))

;; > (removeMults 3 '(2 3 4 5 6 7 8 9 10))
;; (2 4 5 7 8 10)



;; QUESTION 3
;; Function (removeAllMults L) prints the list that contains all the elements of L that are not multiples of each other
;; Base Case: If L is empty, return an empty list
;; Hypothesis: Assume that the function removeMults as defined in Q2 exists, and that the removeAllMults works for problems smaller than the original one
;; Recursive Step: (removeAllMults L) = (cons (car L) (removeAllMults (removeMults (car L) (cdr L))))

> (define (removeAllMults L)
    (cond ((null? L) '())
        (else (cons (car L) (removeAllMults (removeMults (car L) (cdr L)))))))

;; > (removeAllMults '(3 4 6 7 8 10 12 15 20 22 23))
;; (3 4 7 10 22 23)



;; QUESTION 4
;; Function (primes n) returns the list of prime numbers less than n
;; If n is less than 2, return an empty list
;; Else, use the above defined functions by doing (removeAllMults (fromTo 2 n))

> (define (primes n)
  	(cond ((< n 2) '())
        (else (removeAllMults (fromTo 2 n)))))

;; > (primes 40)
;; (2 3 5 7 11 13 17 19 23 29 31 37)



;; QUESTION 5
;; Function (maxDepth L) returns the maximum level of nesting in the list L
;; Base Case: If the list L is null, return 0
;; Hypothesis: Assume that the function (maxDepth (cdr L)) and (maxDepth (car L)) exist and give the maximum depth for the respective sub-lists, which is a problem of size lesser than the original one
;; Recursive Step: If the first element of the list is a list itself, then (maxDepth L) = (max (+ 1 (maxDepth (car L))) (maxDepth (cdr L))), else (maxDepth L) = (maxDepth (cdr L))

> (define (maxDepth L)
    (cond ((null? L) 0)
          ((list? (car L))
           (let ((dcar (+ 1 (maxDepth (car L)))) (dcdr (maxDepth (cdr L)))) (cond ((> dcar dcdr) dcar)
                                                                                  (else dcdr))))
          (else (maxDepth (cdr L)))))

;; > (maxdepth '((0 1) (2 (3 (4 5 (6 (7 8) 9) 10) 11 12) 13) (14 15)))
;; 5



;; QUESTION 6
;; Function (prefix exp) returns the prefix expression for the given infix expression "exp"
;; Base Case: If the expression is empty, return an empty list
;; Hypothesis: Assume that the function (prefix (car exp)) returns the prefix form of the list in the car of exp, and similarly, (prefix (cddr exp)) returns the prefix evaluation of the elements in the cddr of the list exp
;; Recursive Step: If the list contains a single element, (prefix exp) = (car exp), else (prefix exp) = (list (cadr exp) (prefix (car exp)) (prefix (cddr exp)))

> (define (prefix exp)
    (if (null? exp) '()
  		(if (list? exp)
      		(if (null? (cdr exp))
          		(car exp)
          		(list (cadr exp)
                	(prefix (car exp))
                	(prefix (cddr exp))))
        	exp)))

;; > (prefix '((3 * 4) + (5 - 6) * 7))
;; (+ (* 3 4) (* (- 5 6) 7))



;; QUESTION 7
;; Function (composition fns) returns the function that comprises of the composition of the functions given in the List fns
;; Base Case: We do not consider the case where the list of functions is empty, because according to the question, the list can be assumed to contain at least 1 element. So the base case is when the list has only 1 element, then we return that element.
;; Hypothesis: Assume that (composition (cdr fns)) returns the composition function of the remaining functions of the list "fns"
;; Recursive Step: Return lambda function which is the composite function of the 1st element applied on the rest of the list whenever there is more than 1 element in the list, by calling composition (cdr fns)

> (define (composition fns)
  	(let((f1 (car fns)))
    	(if (null? (cdr fns))
       		f1
        	(let ((fi (composition (cdr fns))))
          		(lambda (x) (f1 (fi x)))))))

;; > (define f (composition (list (lambda (x) (+ x 1)) (lambda (x) (* x 2)))))
;; > (f 3)
;; 7



;; QUESTION 8
;; Function (bubble-to-nth L N) bubbles up the largest number in the 1st n elements to the nth position
;; Base Case: If n is 1, we return the list
;; Hypothesis: Assume that (bubble-to-nth (cdr L) (- N 1)) returns the result of bubbling up the largest element from the cdr L to the N-1th position
;; Recursive Step: If the 1st element < 2nd element then, (bubble-to-nth L N) = (cons (car L) (bubble-to-nth (cdr L) (- N 1))), else (bubble-to-nth L N) = (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1)))

> (define (bubble-to-nth L N)
    (if (= N 1) L
        (if (null? (cdr L))
            L
            (if (< (car L) (cadr L))
                (cons (car L) (bubble-to-nth (cdr L) (- N 1)))
                (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- N 1)))))))

;; > (bubble-to-nth '(1 6 2 3 5 4 8 0) 6)
;; (1 2 3 5 4 6 8 0)



;; QUESTION 9
;; Function (b-s L N) returns the list with its first n elements in sorted order
;; Base Case: If N is 1, then return (bubble-to-nth L N)
;; Hypothesis: Assume that the function bubble-to-nth exists as stated above, and that (b-s (bubble-to-nth L N) (- N 1)) will return the list with its N-1 elements in sorted order
;; Recursive Step: If N > 1 then (b-s L N) = (b-s (bubble-to-nth L N) (- N 1))

> (define (b-s L N)
  (if (= N 1) (bubble-to-nth L N)
      (b-s (bubble-to-nth L N) (- N 1))))

;; > (b-s '(1 6 2 3 5 4 8 0) 6)
;; (1 2 3 4 5 6 8 0)



;; QUESTION 10
;; Function (bubble-sort L) returns the list L in sorted order, using the function (b-s L N) defined above

> (define (bubble-sort L)
    (if (null? L) '()
        (b-s L (length L))))

;; > (bubble-sort '(1 6 2 3 5 4 8 0))
;; (0 1 2 3 4 5 6 8)