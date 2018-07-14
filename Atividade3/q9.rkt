#lang racket

(require math/number-theory)
(require srfi/1)


(define (compare-heads? p1 p2) (= (car p1) (car p2)))
(define (list-of-heads lista) (map car lista))

(define (mdc x y)
    (let ([fatores-x (factorize x)] [fatores-y (factorize y)])
        (foldl 
            + 
            (list-of-heads 
                (lset-intersection compare-heads? fatores-x fatores-y)))))

(define fx (factorize 6))
(define fy (factorize 60))
(println fx)
(println fy)

(println "ola")
(println (lset-intersection compare-heads? fx fy))
(print (mdc 6 60))

