#lang racket 

(require "primes.rkt")

(define args (current-command-line-arguments))


(define arg0 (string->number (vector-ref args 0)))
(define arg1 (vector-ref args 1))

(define (abre-arquivo-in arq) (open-input-file arq))
(define (abre-arquivo-out arq) (open-output-file arq #:exists 'append))

(define numero-alvo arg0)
(define arquivo-alvo arg1)

(define file-out (abre-arquivo-out arquivo-alvo))

(define (escreve t) 
    (write t file-out))

(escreve (gen-primes numero-alvo))