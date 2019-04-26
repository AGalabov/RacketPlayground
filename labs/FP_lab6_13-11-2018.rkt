#lang racket

(define (id x) x)
(define (comp f g)
  (lambda(x) (f (g x))))

;---------task1---------

(define (compose . funcs)
    (foldr comp id funcs))

(define (double x) (* 2 x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define f (compose double square inc)) ; 2 * (x + 1)^2
;(f 3) ; -> 32
;(f 4) ; -> 50

;---------task2---------
(define (flip fn)
  (lambda args
    (apply fn (foldl cons '() args))))

(define list^ (flip list))
;(list^ 1 2 3) ; -> (3 2 1)

(define rev-minus (flip -))
;(- 1 2 3); -> -5
;(rev-minus 1 2 3); -> 0


;---------task3A---------
(define (zip x y)
  (if (or (null? x) (null? y))
      '()
      (cons (list (car x)
                  (car y))
            (zip (cdr x)
                 (cdr y)))))

;(zip '(1 3 5) '(2 4 6)) ; -> ((1 2) (3 4) (5 6))
;(zip '(1 3 5) '(2 4 6 8)) ; -> ((1 2) (3 4) (5 6))

;---------task3B---------
(define (zip-with fn x y)
  (if (or (null? x) (null? y))
      '()
      (cons (fn (car x)
                  (car y))
            (zip-with fn
                      (cdr x)
                      (cdr y)))))

;(zip-with + '(1 3 5) '(2 4 6)) ; -> (3 7 11)
;(zip-with + '(1 3 5) '(2 4 6 8)) ; -> (3 7 11)


;---------task3C---------
(define (zip-with-full fn . lists)
  (if (or (null? lists)
          (not (null? (filter null? lists))))
      '()
      (cons (apply fn (map car lists))
            (apply zip-with-full fn (map cdr lists)))))

;(zip-with-full +) ; -> ()
;(zip-with-full + (list 1 2 3) (list 4 5 6) (list 7 8 9)) ; -> (12 15 18)
;(zip-with-full cons (list 1 3 5) (list 2 4 6 8 10)) ; -> ((1 . 2) (3 . 4) (5 . 6))

;---------task4---------
(define (juxt . fns)
  (lambda args
    (if (null? fns)
        '()
        (cons (apply (car fns) args)
              (apply (apply juxt (cdr fns)) args)))))


(define (dec x) (- x 1))

(define g (juxt inc dec square double)) ; (f x) = (list (inc x) (dec x) (square x) (double x))
;(g 5) ; => (6 4 25 10)

(define h (juxt + *))
;(h 3 4 5) ; => (12 60)