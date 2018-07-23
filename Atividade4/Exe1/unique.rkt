#lang racket 

(require racket/list)
(require "primes.rkt")

(define args (current-command-line-arguments))


(define arg0 (vector-ref args 0))
(define arg1 (vector-ref args 1))

(define (abre-arquivo-in arq) (open-input-file arq))
(define (abre-arquivo-out arq) (open-output-file arq #:exists 'append))

(define arquivo-entrada arg0)
(define arquivo-saida arg1)

(define (leia) (read (abre-arquivo-in arquivo-entrada)))
(define (escreve t) (write t (abre-arquivo-out arquivo-saida)))

(define (not-repeated-list lista last-element nova-lista)
      (if (empty? lista) 
            (reverse nova-lista)
            (let ([proximo (car lista)] [resto (cdr lista)])
                  (cond [(empty? lista) nova-lista]
                        [(empty? nova-lista) (not-repeated-list resto proximo (cons proximo nova-lista))]
                        [(= proximo last-element) (not-repeated-list resto proximo nova-lista)]
                        [else (not-repeated-list resto proximo (cons proximo nova-lista))]))))
                  
(escreve (not-repeated-list (leia) -1 '()))