#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))


(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l)
          (foldr op nv (cdr l)))))


(define (foldl-lectures op nv l)
  (if (null? l)
      nv
      (foldl-lectures op (op nv (car l)) (cdr l))))


(define (foldl-racket op nv l)
  (if (null? l)
      nv
      (foldl-racket op (op (car l) nv) (cdr l))))

(define (filter p? l)
  (cond
    ((null? l) l)
    ((p? (car l))
     (cons (car l) (filter p? (cdr l))))
    (else (filter p? (cdr l)))))

