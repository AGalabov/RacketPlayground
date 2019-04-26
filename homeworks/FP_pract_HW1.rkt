#lang racket

;---------HW1task1---------
(define (func n)
  (if (= (remainder n 2) 0)
      (/ n 2)
      (+ (* 3 n) 1)
      )
  )

(define (stopping-time n)
  (if (= (func n) 1)
      1
      (+ 1 (stopping-time (func n)))
      )
  )

;(= (stopping-time 2) 1)
;(= (stopping-time 1) 3)
;(= (stopping-time 7) 16)


;---------HW1task2---------

(define (from-k-ary n k)
  (define (helper exp n)
    (if (= n 0)
        0
        (+ (* (remainder n 10) exp)
           (helper (* exp k) (quotient n 10))
           )
        )
    )
  (helper 1 n)
  )

(define (to-k-ary n k)
  (define (helper exp n)
    (if (= n 0)
        0
        (+ (* (remainder n k) exp)
           (helper (* exp 10) (quotient n k))
           )
        )
    )
  (helper 1 n)
  )

;(= (to-k-ary 215325215 7) 5223143210)
;(= (from-k-ary 211231562734 8) 18428126684)
;(= 123456780 (from-k-ary (to-k-ary 123456780 9) 9))


;---------HW1task3---------
(define (square x) (* x x))
(define (abs x)
  (if (< x 0)
      (- x)
      x)
  )
(define (new-guess guess x)
  (/ (+ guess (/ x guess)) 2)
  )

(define epsilon 0.0001)
(define (?close-enough guess x)
  (< (abs (- (square guess) x)) epsilon)
  )

(define (my-sqrt x)
  (define (iter guess)
    (if (?close-enough guess x)
        guess
        (iter (new-guess guess x))
        )
    )
  (iter 1.0)
  )

;(my-sqrt 5)
;(my-sqrt 9)