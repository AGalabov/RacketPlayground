#lang racket
(require rackunit rackunit/text-ui)

;---------TEST1task1---------
(define (digits-count x)
  (if (< x 10)
      1
      (+ 1 (digits-count (quotient x 10)))
      )
  )

(define (digit-at i x)
  (if (= i 0)
      (remainder x 10)
      (digit-at (- i 1) (quotient x 10))
      )
  )

(define (middle-digit x)
  (define count (digits-count x))
  (if (= (remainder count 2) 0)
      -1
      (digit-at (quotient count 2) x)
      )
  )

(define middle-digit-tests
  (test-suite
   "Tests for middle-digit"

   (check = (middle-digit 0) 0)
   (check = (middle-digit 1) 1)
   (check = (middle-digit 42) -1)
   (check = (middle-digit 452) 5)
   (check = (middle-digit 4712) -1)
   (check = (middle-digit 47124) 1)
   (check = (middle-digit 1892364) 2)
   (check = (middle-digit 38912734) -1)))

(run-tests middle-digit-tests)

;(middle-digit 0)
;(middle-digit 12345)
;(middle-digit 452)
;(middle-digit 4712) 
;(middle-digit 123456)

;---------TEST1task2---------
(define (my-and x y)
  (if x y #f)
  )

(define (stays? list f)
  (foldr my-and
         #t
         (map (lambda (x) (member (f x) list))
              list))
  )

(define (endomorph? op f x y)
  (= (op (f x)
         (f y))
     (f (op x y)))
  )

(define (for-all-next? op f x list)
  (foldr my-and
         #t
         (map (lambda (next) (endomorph? op f x next)) list))
  )

(define (applied? list op f)
  (if (null? list); (null? (cdr list)))
      #t
      (and (for-all-next? op f (car list) (cdr list));may not be associative
           (applied? (cdr list) op f)))
  )

(define (endomorphism? list op f)
  (and (stays? list f) (applied? list op f))
   )

(define list1 (list 1 2 3 4))
(define (1+ x) (+ x 1))
(define (id x) x)

(define endomorphism?-tests
  (test-suite
   "Tests for endomorphism?"

   (check-true (endomorphism? '() + (lambda (x) (remainder x 3))))
   (check-true (endomorphism? '(0 1 4 6) + (lambda (x) x)))
   (check-true (endomorphism? '(0 1 4 6) + (lambda (x) (remainder x 3))))
   (check-false (endomorphism? '(0 1 4 5 6) + (lambda (x) (remainder x 3))))
   (check-false (endomorphism? '(0 1 4 6) expt (lambda (x) (+ x 1))))))

(run-tests endomorphism?-tests)

;(is-em? list1 + id)
;(is-em? (list 0 1 4 6) + (lambda (x) (remainder x 3)))
;(is-em? list1 + 1+)

;---------TEST1task3---------
(define (test? f g x y)
  (and (= (f x) (g x))
       (= (f y) (g y))
       )
  )

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ a 1) b))
      )
  )

(define (my-or x y)
  (if x #t y)
  )

(define (in-next? f g x list)
  (foldr my-or
         #f
         (map (lambda (next) (test? f g x next)) list))
  )

(define (meet-twice? f g a b)
  (define my-list (from-to a b))
  (define (meetTwice-help? list)
    (if (null? list)
        #f
        (or (in-next? f g (car list) (cdr list))
            (meetTwice-help? (cdr list)))))
  (meetTwice-help? my-list)
  )

(define meet-twice?-tests
  (test-suite
   "Tests for meet-twice?"

   (check-true (meet-twice? (lambda (x) x) (lambda (x) x) 0 5))
   (check-true (meet-twice? (lambda (x) x) sqrt 0 5))
   (check-false (meet-twice? (lambda (x) x) (lambda (x) x) 42 42))
   (check-false (meet-twice? (lambda (x) x) (lambda (x) (- x)) -3 1))))

(run-tests meet-twice?-tests)

;(meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1)
;(meetTwice? (lambda(x)x) sqrt 0 5)

;---------TEST1task4---------
(define (next-look-and-say y)
  (define (helper count x  my-list)
    (if (null? my-list)
        (list count x)
        (if (= (car  my-list) x)
            (helper (1+ count) x (cdr  my-list))
            (append (list count x)
                  (helper 1 (car  my-list) (cdr  my-list)))
            )
        )
    )
  (if (empty? y)
      '()
      (helper 1 (car y) (cdr y))
      )
  )

(define next-look-and-say-tests
  (test-suite
   "Tests for next-look-and-say"

   (check-equal? (next-look-and-say '()) '())
   (check-equal? (next-look-and-say '(1)) '(1 1))
   (check-equal? (next-look-and-say '(1 1 2 3 3)) '(2 1 1 2 2 3))
   (check-equal? (next-look-and-say '(1 1 2 2 3 3 3 3)) '(2 1 2 2 4 3))))

(run-tests next-look-and-say-tests)

;(next-look-and-say (list 1 1 2 3 3 4 3))
