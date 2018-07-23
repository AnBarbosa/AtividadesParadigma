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

