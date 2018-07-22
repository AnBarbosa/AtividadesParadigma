#lang racket

(require math/number-theory)
(require srfi/1)
(provide mdc)
(provide multiplica-elementos-na-lista)

(define (compare-heads? p1 p2) (= (car p1) (car p2)))
(define (resto-zero? num div) (= (modulo num div) 0))

(define (list-of-heads lista) (map car lista))
               ; (println lista)
               ; (println (map car lista))
               ; (map car lista))

(define (multiplica-elementos-na-lista lst) (if (list? lst) (foldl * 1 lst) lst))


(define (mdc-custoso x y)
    (let ([fatores-x (factorize x)] [fatores-y (factorize y)])
        (multiplica-elementos-na-lista 
                                        (list-of-heads 
                                                (lset-intersection compare-heads? fatores-x fatores-y)))))


(define (mdc-economico x y)
    (multiplica-elementos-na-lista (lista-primos-comuns x y)))

(define (lista-primos-comuns x y)
    (cond [(> x y) (primos-divisores-comuns x y (list-of-heads(factorize x)) '(1))]
          [else (primos-divisores-comuns x y (list-of-heads(factorize y)) '(1) )]))


(define (proximo-primo p-list)
  (cond [(pair? p-list) (car p-list)]
        [(number? p-list) p-list]
        [else +inf.0]))

(define (resto-da-lista-de-primos plist) (if (pair? plist) (cdr plist) plist))

(define (primos-divisores-comuns x y lista-de-primos lista-divisores)
    ( let ([primo (proximo-primo lista-de-primos)])
     (cond [ (or (> primo x) (> primo y)) lista-divisores]
           [(and (resto-zero? x primo) (resto-zero? y primo)) 
                                                    (primos-divisores-comuns 
                                                    x 
                                                    y 
                                                    (resto-da-lista-de-primos lista-de-primos) 
                                                    (cons primo lista-divisores)) ]
           [else (primos-divisores-comuns 
                                                    x 
                                                    y 
                                                    (resto-da-lista-de-primos lista-de-primos) 
                                                    lista-divisores) ])))
 
#| mdc(x, 0) = x; (mdc x, y) = mdc(y, (resto x/y)) |#


(define (mdc-euclides x y)
    (cond [(= x 0) (if (= y 0) +inf.0 y)]
          [(= y 0) x] ; já sei que x não pode ser zero, pois a condição acima seria ativada.
         ; [(or (= x 1) (= y 1)) 1]
          [(> y x) (mdc-euclides y x)]
          [else (mdc-euclides y (modulo x y))]))

#|
(println (mdc-custoso 13 27))
(println (mdc-custoso 6 60))
(println (mdc-custoso 70 98))


(println (mdc-economico 13 27))
(println (mdc-economico 6 60))
(println (mdc-economico 70 98))


(println (mdc-euclides 13 27))
(println (mdc-euclides 6 60))
(println (mdc-euclides 70 98))


(println (mdc '(13 27)))
(println (mdc '(6 60)))
(println (mdc '(70 98)))
(println (mdc '(70 98 686)))

|#

(define (mdc lista)
  (cond [(pair? lista) (if (null? (cdr lista)) (car lista) (mdc-helper (car lista) (mdc (cdr lista))))]
        [else lista]))

(define (mdc-helper num lista)
    (if (pair? lista) (mdc-euclides num (car lista)) (mdc-euclides num lista)) )       






