#lang racket

#| Um inteiro positivo é perfeito quanto a soma dos seus 
divisores (exceto o próprio número) é igual ao próprio 
número, e.g. 6 = 1 + 2 + 3 e 28 = 1 + 2 + 4 + 7 + 14. 
Construa o procedimento perfeito? (deve ser um predicado
 com esse nome) que determine se um um número n é
perfeito. 
|#

(require quickcheck)

#| A solução está na linha 62:
;;;**************** SOLUÇÃO  *******************
;   (define (perfeito? numero)
;       (= 
;          (foldr + 0 (divisores-de numero))
;          numero))
;;;*********************************************
|#

(define (gera-lista n) (build-list n values))


(define prop-gera-lista-tem-n-itens
    (property ([n arbitrary-natural])
    (= (length (gera-lista n)) n)))

(quickcheck prop-gera-lista-tem-n-itens)

(define (soma-lista lista)
        (if (empty? lista)
            0
            (+ (car lista) (soma-lista (cdr lista)))))

(define (soma-sequencia n)
  (*  (+ 1 n)  (/ n 2)))
   
(define prop-soma-lista
    (property ([n arbitrary-natural])
    (if (< n 2)
        #true         
        (= (soma-lista (gera-lista (+ n)))
           (soma-sequencia(- n 1))))))

(quickcheck prop-soma-lista)




(define (divisores-de alvo)
  (filter
       (lambda (n)
             (if (= n 0) #false (= (modulo alvo n) 0)))
       (gera-lista alvo)))

(define prop-divisores
  (property ([n arbitrary-natural])
            (if (< n 2)
               #true
               (andmap
                  (lambda (num) (= (modulo n num) 0))
                  (divisores-de n)))))

(quickcheck prop-divisores)


;;;**************** SOLUÇÃO  *******************
(define (perfeito? numero)
    (= (foldr + 0 (divisores-de numero)) numero))
;;;*********************************************
(define prop-perfeito
  (property ([n arbitrary-natural])
            (if (< n 2)
               #true
               (if
                (perfeito? n)
                (= (soma-lista (divisores-de n)) n)
                (not (= (soma-lista (divisores-de n)) n))))))

(quickcheck prop-perfeito)



