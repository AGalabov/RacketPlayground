#lang racket

;---------HW2task1---------
(define (my-and x y)
  (if x y #f)
  )

(define (all? p? xs)
  (foldr my-and #t (map p? xs))
  )

;(all? even? '(2 4 6))   -> #t
;(all? even? '(1 2 4 6)) -> #f
;(all? even? '())        -> #t

;---------HW2task2---------
(define (my-or x y)
  (if x #t y)
  )

(define (any? p? xs)
  (foldr my-or #f (map p? xs))
  )

;(any? even? '(1 2 4 6)) -> #t
;(any? odd? '(2 4 6))    -> #f
;(any? even? '())        -> #f

;---------HW2task3---------
(define (concat xxs)
  (foldr append '() xxs)
  )

;(concat '((1 2 3) (4 5 6) (7 8 9))) -> '(1 2 3 4 5 6 7 8 9)
;(concat '())                        -> '()
;(concat '(() () () () () () ()))    -> '()

;---------HW2task4---------
(define (rows xss)
  (if (null? xss)
      '()
      (cons (car xss) (rows (cdr xss)))
      )
  )

;(rows '((1 2 3)
;        (4 5 6)
;        (7 8 9))) ->  '((1 2 3) (4 5 6) (7 8 9))

;---------HW2task5---------
(define (columns xss)
  (if (all? null? xss)
      '()
      (cons (map (lambda (l) (car l)) xss)
            (columns (map (lambda (l) (cdr l)) xss)))
      )
  )

;(columns '((1 2 3)
;        (4 5 6)
;        (7 8 9))) ->  '((1 4 7) (2 5 8) (3 6 9))


;---------HW2task6---------
(define (elem-at i list)
  (if (= i 0)
      (car list)
      (elem-at (- i 1) (cdr list)))
  )
(define (matrix-ref xss i j)
  (elem-at j (elem-at i xss))
  )

;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 1 1) -> 5
;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 1 0) -> 4
;(matrix-ref '((1 2 3)
;              (4 5 6)
;              (7 8 9)) 0 2) -> 3

;---------HW2task7---------
(define (set xs i x)
  (if (null? xs)
      '()
      (cons (if (= i 0)
                x
                (car xs))
           (set (cdr xs) (- i 1) x))
      )
  )

;(set '(1 2 3) 2 1337);   -> '(1 2 1337)
;(set '(1 2 3 4 5) 0 42); -> '(42 2 3 4 5)

;---------HW2task8---------
(define (place xss i j x)
  (set xss i (set (elem-at i xss) j x))
  )

;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 1 1 42)
;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 1 0 69)
;(place '((1 2 3)
;         (4 5 6)
;         (7 8 9)) 0 2 1337)

;---------HW2task9---------
(define (diag xss)
  (if (null? xss)
      '()
      (cons (car (car xss))
            (diag (cdr (map cdr xss)))
            )
      )
  )

;(diag '((1 2 3)
;        (4 5 6)
;        (7 8 9)))
;(diag '((1)))
;(diag '((1 0)
;        (0 1)))

;---------HW2task10---------
(define (diags xss)
  (cons (diag xss)
        (list (diag (map reverse xss)))
        )
  )

;(diags '((1 0)
;         (0 1)))
;(diags '((1 2 3)
;         (4 5 6)
;         (7 8 9)))

;---------HW2task11---------
(define (map-matrix f xss)
  (map (lambda (l)
         (map f l))
       xss)
  )  

(define (1+ x) (+ 1 x))
(define (id x) x)
(define (const x) (lambda (y) x))
  
;(map-matrix id '((1337)))
;(map-matrix 1+ '((1 0)
;                (0 1)))
;(map-matrix (const 69) '((1 2 3)
;                         (4 5 6)
;                         (7 8 9)))

;---------HW2task12---------
(define (filter-matrix p? xss)
  (map (lambda (l)
         (filter p? l))
       xss)
  )

;(filter-matrix odd?  '((1 2 3)
;                       (4 5 6)
;                       (7 8 9)))
;(filter-matrix zero? '((1 0)
;                       (0 1)))
;(filter-matrix zero? '((1 2 3)
;                       (4 5 6)
;                       (7 8 9)))

;---------HW2task13---------
(define (zip-with f xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (f (car xs) (car ys))
            (zip-with f (cdr xs) (cdr ys))
            )
      )
  )

;(zip-with cons '(1 2 3) '(4 5 6));              -> '((1 . 4) (2 . 5) (3 . 6))
;(zip-with cons '(1) '(4 5 6));                  -> '((1 . 4))
;(zip-with + '(60 1300 40) '(9 37 2));           -> '(69 1337 42)
;(zip-with string-append '("y" "y") '("o" "o")); -> '("yo" "yo")

;---------HW2task14---------
(define (zip-matrix xss yss)
   (if (or (null? xss)
          (null? yss))
      '()
      (cons (zip-with cons (car xss) (car yss))
            (zip-matrix (cdr xss) (cdr yss))
            )
      )
  )

;(zip-matrix '((1 2))
;            '((3 4)))
;(zip-matrix '((1 0)
;              (0 1))
;            '((6 9)
;              (9 6)))