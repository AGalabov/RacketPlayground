#lang racket

;----------task1----------
(define (factorial-iter n)
  (define (fact-help num acc)
    (if (> num n)
        acc
        (fact-help (+ num 1) (* acc num))))
  (fact-help 1 1))

(factorial-iter 0)
(factorial-iter 5)

;----------task2----------
(define (sum-iter start end)
  (define (sum-iter-help sum counter)
    (if (> counter end)
       sum
       (sum-iter-help (+ sum counter) (+ counter 1))))
  (sum-iter-help 0 start))

(sum-iter 1 10)


;----------task3----------
(define (expt-iter x n)
  (define (expt-iter-help expt counter)
    (if (> counter n)
       expt
       (expt-iter-help (* expt x) (+ counter 1))))
  (expt-iter-help 1 1))

(expt-iter 2 5)

;----------task4a----------
(define (count-digits-recursive n)
    (if (= (quotient n 10) 0)
       1
       (+ 1 (count-digits-recursive (quotient n 10)))))

(count-digits-recursive 1225)

;----------task4b----------
(define (count-digits-iter n)
  (define (count-digits-iter-help n count)
    (if (= (quotient n 10) 0)
       count
       (count-digits-iter-help (quotient n 10) (+ count 1))))
  (count-digits-iter-help n 1))

(count-digits-iter 1225)

;----------task5a----------
(define (sum-digits-recursive n)
    (if (= (quotient n 10) 0)
       n
       (+ (remainder n 10) (sum-digits-recursive (quotient n 10)))))

(sum-digits-recursive 1225)

;----------task5b----------
(define (sum-digits-iter n)
  (define (sum-digits-iter-help n sum)
    (if (= n 0)
        sum
       (sum-digits-iter-help (quotient n 10) (+ sum (remainder n 10)))))
  (sum-digits-iter-help n 0))

(sum-digits-iter 1225)

;----------task6a----------
(define (reverse-digits-recursive n)
    (if (= n 0)
       0
       (+ (* (expt-iter 10 (- (count-digits-iter n) 1)) (remainder n 10)) (reverse-digits-recursive (quotient n 10)))))

(reverse-digits-recursive 12252)

;----------task6b----------
(define (reverse-digits-iter n)
  (define (reverse-digits-iter-help res pow n)
    (if (= n 0)
       res
       (reverse-digits-iter-help (+ res (* pow (remainder n 10))) (/ pow 10) (quotient n 10))))
  (reverse-digits-iter-help 0 (expt-iter 10 (- (count-digits-iter n) 1)) n))

(reverse-digits-iter 12252)

;----------task7a----------
(define (count-devisors-recursive n)
  (define (count-devisors-recursive-help devisor)
    (if (> devisor n)
        0
        (+ (if (= (remainder n devisor) 0)
               1
               0)
           (count-devisors-recursive-help (+ devisor 1)))))
  (count-devisors-recursive-help 1))
          
(count-devisors-recursive 10)

;----------task7b----------
(define (count-devisors-iter n)
    (define (count-devisors-iter-help devisor count)
      (if (> devisor n)
          count
          (if (= (remainder n devisor) 0)
              (count-devisors-iter-help (+ devisor 1) (+ count 1))
              (count-devisors-iter-help (+ devisor 1) count))))
  (count-devisors-iter-help 1 0))
          
(count-devisors-iter 10)

;----------task8a----------
(define (sum-divisors-iter n)
  (define (sum-divisors-iter-help sum divisor)
    (if (> divisor n)
       sum
       (if (= (remainder n divisor) 0)
           (sum-divisors-iter-help (+ sum divisor) (+ divisor 1))
           (sum-divisors-iter-help sum (+ divisor 1)))))
  (sum-divisors-iter-help 0 1))

(sum-divisors-iter 10)

;----------task8b----------
(define (sum-divisors-recursive n)
  (define (sum-divisors-recursive-help divisor)
    (if (> divisor n)
       0
       (+ (sum-divisors-recursive-help (+ divisor 1))
          (if (= (remainder n divisor) 0)
              divisor
              0))))
  (sum-divisors-recursive-help 1))

(sum-divisors-recursive 10)

;----------task9----------
(define (prime? x)
  (or (= x 1) (= (sum-divisors-iter x) (+ x 1))))

(prime? 0)
