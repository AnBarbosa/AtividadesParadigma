#lang racket
(require memoize)
(provide seno)
(define (proximo-suficiente? a b delta)
        (< (abs (- a b)) delta))

(define (fatorial n) (fatorial-helper n 1))
(define (fatorial-helper n acumulador)
        (cond   [ (= n 0) 1]
                [ (= n 1) acumulador]
                [else (fatorial-helper 
                                    (- n 1) 
                                   (* n acumulador))]))

(define (termo x n)
        (let [(valor (/      (* (expt -1 n) (expt x (+ (* 2 n) 1)))
                (fatorial (+ (* 2 n) 1))))]
         ; (printf "x = ~v n = ~v valor = ~v \n" x n valor)
          valor))


(define/memo (seno-iter angulo valorAnterior valorAtual erroDesejado  iteracao)
        (if (proximo-suficiente? valorAnterior valorAtual erroDesejado)
                valorAtual
                (seno-iter 
                        angulo 
                        valorAtual 
                        (+ valorAtual (termo angulo iteracao)) 
                        erroDesejado 
                        (+ iteracao 1))))

(define (seno angulo erro) (seno-iter angulo 100 angulo erro 1))

