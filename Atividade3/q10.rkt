#lang racket

(require "q9.rkt")
(require "primes.rkt")

(define (mdc lista)
    (if (pair? lista) (if (null? (cdr lista)) (car lista) (mdc-lista lista (gen-primes (maior-de lista)) '(1)) lista))
    
(define (resto-um? num div) (= 1 (modulo num div)))
(define (divide-se-divisor a b) (if (= 0 (modulo a b)) (/ a b) a))

(define (mdc-lista lista primos resposta)
    (let ([(sobra-um? (lambda (num) (resto-um num (car primos))))]
          [(divide-se-der (lambda (num) (divide-se-divisor num (car primos))))])
        
          (cond [andmap sobra-um lista] (multiplica-elementos-na-lista resposta)]
              [else mdc-lista (map divide-se-der lista) (cdr primos) (cons (car primos) resposta)])))