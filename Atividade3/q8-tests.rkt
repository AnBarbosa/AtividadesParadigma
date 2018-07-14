#lang racket

(require "q8.rkt")

(require quickcheck)

(define teste-divisivel-resultado-da-divisao
    (property ([n arbitrary-natural] [m arbitrary-natural])
    (cond [(= m 0) #t]
          [(= n 0) #t]
          [(divisivel? n m) (integer? (/ n m))]
          [(not (divisivel? n m)) (not (integer? (/ n m)))]
         [else #t])))
            

(define prop-possiveis-divisores-menores-que-n
    (property ([n arbitrary-natural])
        (cond [(= n 0) #t]
              [(= n 1) #t]
              [else (andmap  (lambda (v) (not (> v n))) (possiveis-divisores n))])))

;(println (primos-ate 1051))

(define prop-primos-ate-menor-que-n-impares
    (property ([n (choose-integer 2 (random 10000))])
            (andmap (lambda (v) (not (> v n))) (primos-ate n))))


(define prop-marsene
    (property ([n arbitrary-natural])
    (if (primo? n) (primo? (marsenne n)) #t)))
            

(quickcheck teste-divisivel-resultado-da-divisao); ok
(quickcheck prop-primos-ate-menor-que-n-impares) ; ok
(quickcheck  prop-possiveis-divisores-menores-que-n); ok
(quickcheck prop-marsene); ok, falha para n=23