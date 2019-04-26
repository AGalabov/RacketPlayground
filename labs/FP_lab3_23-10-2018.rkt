#lang racket

;accumulator
(define (accumulate op nv start end next term)
  (if (> start end)
      nv
      (op (term start)
          (accumulate op nv (next start) end next term)
          )
      )
  )

(define (1+ x) (+ x 1))

;----------task5----------
(define (count predicate a b)
  (accumulate + 0 a b 1+ (lambda (x) (if (predicate x) 1 0))))

(define (even? x)
  (= (remainder x 2) 0))

;(count even? 1 11)

;;;from 16-10 tasks 4b,6b
(define (expt-iter x n)
  (define (expt-iter-help expt counter)
    (if (> counter n)
       expt
       (expt-iter-help (* expt x) (+ counter 1))))
  (expt-iter-help 1 1))

(define (count-digits-iter n)
  (define (count-digits-iter-help n count)
    (if (= (quotient n 10) 0)
       count
       (count-digits-iter-help (quotient n 10) (+ count 1))))
  (count-digits-iter-help n 1))

(define (reverse-digits-iter n)
  (define (reverse-digits-iter-help res pow n)
    (if (= n 0)
       res
       (reverse-digits-iter-help (+ res (* pow (remainder n 10))) (/ pow 10) (quotient n 10))))
  (reverse-digits-iter-help 0 (expt-iter 10 (- (count-digits-iter n) 1)) n))

;----------task6----------
(define (palindrome? x)
  (= (reverse-digits-iter x) x))

;(count palindrome? 1 10)

;----------task7----------
(define (exists? predicate a b)
  (> (count predicate a b) 0)
  )

;(exists? palindrome? 123 200)

;----------task8----------
(define (for-all? predicate a b)
  (= (count predicate a b) (1+ (- b a)))
  )

;(for-all? palindrome? 3 9)

;----------task9----------
(define (double f)
  (lambda (x) (f (f x))))

(define 2+ (double 1+))

(define (square x) (* x x))

(define quad (double square))

;(2+ 40)
;(quad 3)

;----------task10----------
(define (compose f g)
  (lambda (x) (f (g x))))

;((compose 1+ quad) 2)
;((compose quad 1+) 2)

;----------task11----------
(define (repeated f n)
  (accumulate compose (lambda (x) x) 1 n 1+ (lambda (x) f)))

;((repeated square 2) 5) 
  
