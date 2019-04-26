#lang racket

(define (succ x) (+ x 1))
(define (pred x) (- x 1))

(define (my-plus x y)
  (if(> y 0) (my-plus (succ x) (pred y)) x))

;x<y
(define (my-plus1 x y)
  (if(= x 0) y
     (succ(my-plus1 (pred x) y))))


(define (my-mult x y)
  (if(= x 1) y
     (+ y (my-mult (pred x) y))))

;(my-mult 22 6)

(define (my-pow x y)
  (if(= y 1) x
     (* x (my-pow x (pred y)))))

;(my-pow 2 64)

(define (fast-pow x y)
  (if (= y 1) x
     (if (= (remainder y 2) 0)
        (fast-pow (* x x) (quotient y 2))
        (* x (fast-pow x (pred y)))
      )
   ))

;(fast-pow 2 64)

(define (factorial x)
  ( if(= x 1) x
     (* x (factorial (pred x) ))))

;(factorial 5)

(define (fibonacci x)
  ( if(< x 2) 1
     (+ ( fibonacci (pred x)) (fibonacci (- x 2) ))))

;(fibonacci 7)

(define (add x y) (+ x y)) 

(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))

(define (signum x)
  (cond
    ((> x 0) 1)
    ((= x 0) 0)
    ((< x 0) -1)
  ))
