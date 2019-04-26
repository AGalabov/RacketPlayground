#lang racket
(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t)
                 (cons h (delay t)))))

(define (empty-stream? s)
  (equal? s the-empty-stream))

(define head car)

(define (tail s)
  (force (cdr s)))

(define (take n s)
  (if (= n 0)
      '()
      (cons (head s)
            (take (- n 1) (tail s)))))


;----------task1----------
; Kolakoski sequence
(define (helper currentValue nMore queue)
    (if (= nMore 0)
         (if (= currentValue  1)
             (cons-stream 2
                          (helper 2 (cadr (append queue (list 1))) (cdr (append queue (list 1)))))
             (cons-stream 1
                          (helper 1 (cadr (append queue (list 0))) (cdr (append queue (list 0))))))
         (cons-stream currentValue
                      (helper currentValue (- nMore 1) (append queue (list (- currentValue 1)))))))

(define kolakoski
 (helper 1 1 '()))

(take 10 kolakoski)



 