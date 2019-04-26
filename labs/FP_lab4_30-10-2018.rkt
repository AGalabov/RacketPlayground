#lang racket

(define (accumulate op nv start end next term)
  (if (> start end)
      nv
      (op (term start)
          (accumulate op nv (next start) end next term)
          )
      )
  )

(define (compose f g)
  (lambda (x) (f (g x))))


;(cons 1
;      (cons 2
;            (cons 3
;                  (cons 4
;                       (cons 5 1)))))

(define (constructor start end next)
  (accumulate cons '() start end next (lambda (x) x)
  ))


(define (1+ x) (+ x 1))

(define list (constructor 0 10 1+))

;(car list) ;-- returns the head = 1st element
;(cdr list) ;-- returns the tail = list of all elements but first
;(car (cdr (cdr (cdr list)))) ;-- returns the 4th element


;----------task1----------
(define (length l)
  (if (null? l)
      0
      (1+ (length (cdr l)))
      )
)

(length list)

;----------task2----------
(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))
      )
)

(sum list)

;----------task3----------
(define (member l x)
  (if (null? l)
      #f
      (or (= (car l) x) (member (cdr l) x))
      )
)

(member list 3)
(member list 13)

;----------task4----------
(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))
      )
)

(last list)

;----------task5----------
(define (nth l n)
  (define (nth_help count l)
    (if (null? (cdr l))
      'NonExistant
      (if (= count (- n 1))
          (car l)
          (nth_help (1+ count) (cdr l))
      )
     )
   )
  (nth_help 0 l)
)

(nth list 3)
(nth list 12)

;----------task6----------
(define (scale l x)
  (if (null? l)
      '()
      (cons (* (car l) x) (scale (cdr l) x))
      )
)

(scale list 2)

;----------task8----------
(define (add-last l x)
  (if (null? l)
      (cons x '())
      (cons (car l) (add-last (cdr l) x))
      )
)

(add-last list 11)

;----------task7----------
(define (reverse l)
  (if (null? (cdr l))
      (add-last l (car l))
      (add-last (reverse (cdr l)) (car l))
      )
)

(reverse list)

;----------task9----------
(define (append l1 l2)
  (if (null? (cdr l2))
      (add-last l1 (car l2))
      (append (add-last l1 (car l2)) (cdr l2))
      )
)

(define l2 (constructor 11 13 1+))

(append list l2)

;----------task10----------
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map f (cdr l)))
      )
)

(map 1+ list)

;----------task11----------
(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
            (cons (car l) (filter p (cdr l)))
            (filter p (cdr l))
      )
    )
)

(filter even? list)

;----------task12----------
(define (reject p l)
  (if (null? l)
      '()
      (if (p (car l))
          (reject p (cdr l))
          (cons (car l) (reject p (cdr l)))
      )
    )
  ;(filer (compose not p) l) ;-- another way
)

(reject even? list)




