
#lang racket

(provide prime?)
(provide gen-primes-a)
(provide gen-primes-b)
(provide gen-primes)
(provide factorize)

(require racket/trace)

(define (divides a b)
  (= (modulo a b) 0))

(define (prime? n) 
  (prime-c? n))

(define (gen-primes n)
  (gen-primes-b n))

; prime test with sqrt(n) and 6(k+1) form
(define (prime-c? n)
  (cond [(= n 2) #t]
        [(= n 3) #t]
        [else (let loop ([k 1] [i 2] [j 3])
                (cond [(> (* i i) n) #t]
                      [(divides n i) #f]
                      [(divides n j) #f]
                      [else (loop 
                              (+ k 1) 
                              (- (* 6 k) 1) 
                              (+ (* 6 k) 1))]))]))

; prime enumeration with exhaustive trial division
(define (gen-primes-a n)
  (let gen-primes-a-i ([i 2] [p null])
    (cond [(> i n) (reverse p)]
          [(prime? i) (gen-primes-a-i (+ i 1) (cons i p))]
          [else (gen-primes-a-i (+ i 1) p)])))

(define (gen-pair k)
  (list (- (* 6 k) 1) (+ (* 6 k) 1)))

(define (gen-wheel-pairs n)
  (let rec ([k 1])
    (if (= k n)
        null
        (cons (gen-pair k) (rec (+ k 1))))))

; cons in primes only if i is prime
(define (cons-prime i primes)
  (if (prime? i)
      (cons i primes)
      primes))

; prime enumeration relying on 6k+-1 property
(define (gen-primes-b n)
  (cond [(< n 2) null]
        [(= n 2) (list 2)]
        [(= n 3) (list 2 3)]
        [else (let loop ([i 5] [k 2] [primes '(3 2)])
                (cond [(> i n) (reverse primes)]
                      [(= k 2) (loop (+ i k) 4 (cons-prime i primes))]
                      [else (loop (+ i k) 2 (cons-prime i primes))]))]))

; decompose a number into prime factors
(define (factorize n)
  (let loop ([n n] [factors null] [primes (gen-primes (sqrt n))])
    (cond [(< n 2) (reverse factors)]
          [(null? primes) (reverse (cons n factors))]
          [(divides n (car primes)) 
           (loop (quotient n (car primes)) (cons (car primes) factors) primes)]
          [else (loop n factors (cdr primes))])))






; running time experiments

; (time (prime-a? 6700417))
; (time (prime-a? 67280421310721))
; (time (prime-a? 2147483647))

; (display "\n")

; (time (prime-b? 6700417))
; (time (prime-b? 67280421310721))
; (time (prime-b? 2147483647))

; (display "\n")

; (time (prime-c? 6700417))
; (time (prime-c? 67280421310721))
; (time (prime-c? 2147483647))