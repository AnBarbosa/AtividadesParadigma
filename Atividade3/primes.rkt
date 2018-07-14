#lang racket

(provide prime?)
(provide prime-a?)
(provide prime-b?)
(provide divisivel?)



(define (divisivel? a b)
  (= (modulo a b) 0))

(define (prime? n)
  (if (< n 2)
      #f
      (prime-f? n 2)))

; prime  test with exhaustive divisor search
(define (prime-f? n i)
  (cond [(= n i) #t]
        [(divisivel? n i) #f]
        [else (prime-f? n (+ i 1))]))

; prime test with sqrt(n) pruning
(define (prime-a? n)
  (let prime-a-i ([i 2])
    (cond [(> (* i i) n) #t]
          [(divisivel? n i) #f]
          [else (prime-a-i (+ i 1))])))

; prime test with sqrt(n) and even divisor pruning
(define (prime-b? n)
  (cond [(= n 2) #t]
        [(divisivel? n 2) #f]
        [else (let prime-b-i ([i 3])
                (cond [(> (* i i) n) #t]
                      [(divisivel? n i) #f]
                      [else (prime-b-i (+ i 2))]))]))

; (time (prime? 67280421310721))
; (time (prime? 2147483647))
; (time (prime? 6700417))

; (time (prime-a? 6700417))
; (time (prime-a? 67280421310721))
; (time (prime-a? 2147483647))

; (time (prime-b? 6700417))
; (time (prime-b? 67280421310721))
; (time (prime-b? 2147483647))