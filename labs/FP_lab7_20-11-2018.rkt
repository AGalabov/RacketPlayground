#lang racket
;https://github.com/ekaranasuf/fp1819/tree/master/week8
(define example-graph
   '((1 2)
     (2 1 3 4)
     (3 3)
     (4 1 3)
     (5 6)
     (6)))


;----------task3----------
(define (vertices g)
  (if (null? g)
      '()
      (cons (car (car g))
            (vertices (cdr g)))))

;(vertices example-graph)      ;; връща '(1 2 3 4 5 6)

(define (children g v)
  (cdr (assoc v g)))

;(children example-graph 4)    ;; връща '(1 3)

(define (edge? g u v)
  (not (null? (filter (lambda (x) (= x v)) (cdr (assoc u g))))))

;(edge? example-graph 1 5)   ;; връща #f
;(edge? example-graph 5 6)   ;; връща #t


;----------task4----------
(define (parents g v)
  (filter (lambda (u) (edge? g u v)) (vertices g)))

;(parents example-graph 3)   ;; връща '(2 3 4)

;----------task5----------
(define (make-pairs l)
  (cdr (map (lambda (x)
              (cons (car l)
                    x))
              l)))

(define (to-edge-list g)
  (apply append (map make-pairs g)))

;(to-edge-list example-graph)
;; връща
;; '(1 . 2) '(2 . 1) '(2 . 3) '(2 . 4)
;; '(3 . 3) '(4 . 1) '(4 . 3) '(5 . 6)


;----------task11----------
(define-syntax cons-stream        ;; дефинираме специална форма
  (syntax-rules ()
    ((cons-stream head tail) (cons head (delay tail)))))
(define head car)
(define (tail s) (force (cdr s)))

(define (from-to start end)
  (if (> start end)
      '()
      (cons-stream start
                   (from-to (+ start 1) end))))

(define numbers (from-to 1 100))
;(head numbers)          ;; връща 1
;(head (tail numbers))   ;; връща 2

;----------task12----------

(define (take-stream n stream)
  (if (or (null? stream)
          (= n 0))
      '()
      (cons (head stream) (take-stream (- n 1) (tail stream)))))

;(take-stream 10 numbers)


;----------task13----------
(define (add stream1 stream2)
  (if (or (null? stream1)
          (null? stream2))
      '()
      (cons-stream (+ (head stream1)
                      (head stream2))
                   (add (tail stream1)
                        (tail stream2)))))

;(take-stream 6 (add numbers (take-stream 5 numbers)))

;----------task14----------
(define (from n)
  (cons-stream n (from (+ n 1))))

(define rats (from 1))

;(take-stream 100 rats)

;----------task15----------
(define (fibonachi)
  (define (fib fn1 fn2)
    (cons-stream (+ fn1 fn2)
                 (fib fn2 (+ fn1 fn2))))
  (cons-stream 1 (fib 0 1)))

;(take-stream 10 (fibonachi))

;----------task16----------
(define (map-stream f stream)
  (if (null? stream)
      '()
      (cons-stream (f (head stream))
                   (map-stream f (tail stream)))))

;(take-stream 10 (map-stream (lambda (x) (+ x 1)) rats))

(define (filter-stream p? stream)
  (if (null? stream)
      '()
      (if (p? (car stream))
          (cons-stream (head stream)
                       (filter-stream p? (tail stream)))
          (filter-stream p? (tail stream)))))

(define 10even (take-stream 10 (filter-stream even? rats)))
(define 10odd (take-stream 10 (filter-stream odd? rats)))


(define (zip-stream f stream1 stream2)
  (if (or (null? stream1)
          (null? stream2))
      '()
      (cons-stream (f (head stream1)
                      (head stream2))
                   (zip-stream f
                               (tail stream1)
                               (tail stream2)))))

;(take-stream 10 (zip-stream + rats rats))

;----------task17----------
(define (repeat list)
  (define (rep l)
    (if (null? l)
        (rep list)
        (cons-stream (car l) (rep (cdr l)))))
  (rep list))

;(take-stream 10 (repeat (list 1 2 3)))

;----------task18----------
(define (repeat-two list1 list2)
  (define new-list (append list1 list2))
  (define (rep l)
    (if (null? l)
        (rep new-list)
        (cons-stream (car l) (rep (cdr l)))))
  (rep new-list))

;(take-stream 10 (repeat-two (list 1 2) (list 3)))