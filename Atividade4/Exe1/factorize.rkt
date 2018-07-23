#lang racket 

(require "primes.rkt")

(define args (current-command-line-arguments))


(define arg0 (string->number (vector-ref args 0)))
(define arg1 (vector-ref args 1))
(define arg2 (vector-ref args 2))

(define (abre-arquivo-in arq) (open-input-file arq))
(define (abre-arquivo-out arq) (open-output-file arq #:exists 'append))


(define numero-alvo arg0)
(define arquivo-entrada arg1)
(define arquivo-saida arg2)

(define (escreve t) (write t (abre-arquivo-out arquivo-saida)))
(define (leia) (read (abre-arquivo-in arquivo-entrada)))


; Funções no domínio do problema


; decompose a number into prime factors using existing list
(define (factorize-with-list n primes-list)
  (let loop ([n n] [factors null] [primes primes-list])
    (cond [(< n 2) (reverse factors)]
          [(null? primes) (reverse (cons n factors))]
          [(divides n (car primes)) 
           (loop (quotient n (car primes)) (cons (car primes) factors) primes)]
          [else (loop n factors (cdr primes))])))



(escreve (factorize-with-list numero-alvo (leia)))
