#lang racket


(define (gen-primes n)
  (cond [(< n 2) null]
        [(= n 2) (list 2)]
        [(= n 3) (list 2 3)]
        [else (let loop ([i 5] [k 2] [primes '(3 2)])
                (cond [(> i n) (reverse primes)]
                      [(= k 2) (loop (+ i k) 4 (cons-prime i primes))]
                      [else (loop (+ i k) 2 (cons-prime i primes))]))]))