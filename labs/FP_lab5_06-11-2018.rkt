#lang racket

;TASK1a
(define (length x)
  (if (< x 10)
        1
        (+ 1
           (length (quotient x 10)))
        )
    )

(define (pow x n)
  (if (= n 0)
        1
        (* x (pow x (- n 1)))
        )
    )

(define (reverse-num x)
  (define (reverse-num-helper x pow)
    (if (< x 10)
        x
        (+ (* (remainder x 10) pow)
           (reverse-num-helper (quotient x 10) (/ pow 10)))
        )
    )
  (reverse-num-helper x (pow 10 (- (length x) 1)))
  )

(define (diffReverse n)
  (- n (reverse-num n)))

;(diffReverse 7641)
;(diffReverse 123)
;(diffReverse 7)

;TASK1b
(define (count-digit d x)
  (if (< x 10)
      (if (= x d)
          1
          0)
      (+ (if (= (remainder x 10) d)
             1
             0)
         (count-digit d (quotient x 10)))))


(define (sortDigits n)
  (define (helper power max-digit used sum)
    (define count (count-digit max-digit n))
    (if (= max-digit 0)
        sum
        (if (or (= count 0)
                (= count used))
            (helper power
                    (- max-digit 1)
                    0
                    sum)
            (helper (/ power 10)
                    max-digit
                    (+ used 1)
                    (+ sum
                       (* max-digit power))))))
(helper (pow 10 (- (length n) 1)) 9 0 0)
  )

;(sortDigits 6174)
;(sortDigits 9912939)
;(sortDigits 0)
;(sortDigits 123456789)


;TASK2

(define (calculate-n x f g n)
  (if (= n 0)
      x
      (f (calculate-n x g f (- n 1)))
      )
  )

(define (permutable? a b f g)
  (define (permutable-help? a b f g)
    (if (> a b)
        #t
        (if (not (= (calculate-n a f g a) (calculate-n a g f a)))
            #f
            (permutable-help? (+ a 2) b f g))))
  (if (odd? a)
      (permutable-help? (+ a 1) b f g)
      (permutable-help? a b f g))
  )

;(permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (* x x x)))
;(permutable? 1 9 (lambda (x) (+ x 1)) (lambda (x) (- x 2)))
;(permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (/ x 2)))
;(permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (+ 100 x)))


;TASK3a
(define test '((24 . 25) (90 . 110) (0 . 100) (10 . 109) (1 . 3) (-4 . 2)))
(define test2 (list (cons 24 25) (cons 90 110) (cons 0 100) (cons 10 109) (cons 1 3) (cons -4 2) (cons 1 3) (cons 2 100) (cons 0 100) (cons 0 3)))

(define (get-distance pair)
  (- (cdr pair) (car pair)))

(define (longest-interval list)
  (define (helper curr-max a b list1)
    (if (null? list1)    
        (cons a b)
        (if (> (get-distance (car list1)) curr-max)
            (helper (get-distance (car list1)) (car (car list1)) (cdr (car list1)) (cdr list1))
            (helper curr-max a b (cdr list1))
            )
        )
    )
  (helper -1 0 0 list)
  )

(define (subSet? pair1 pair2)
  (and (>= (car pair2) (car pair1))
       (<= (cdr pair2) (cdr pair1))))

(define (longest-interval-subset-a il)
  (define longest (longest-interval il))
  (filter (lambda (x)
            (subSet? longest
                     x))
          il))

;(longest-interval-subset-a test)

;TASK3b
(define (quick-sort il)
  (if (null? il)
      '()
      (append (quick-sort
               (filter (lambda (x)
                         (< (car x)
                            (car (car il))))
                       (cdr il)))
              (list (car il))
              (quick-sort
               (filter (lambda (x)
                         (>= (car x)
                             (car (car il))))
                       (cdr il))))
      )
  )

(define (longest-interval-subset il)
  (quick-sort (longest-interval-subset-a il)))

;(longest-interval-subset test)
;(longest-interval-subset test2)


