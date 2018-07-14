#lang racket
(require memoize)

#| Desejamos uma função T(linha, coluna) que retorne o valor 
do número naquela posição dentro de um trianglo de pascal.

Sabemos que, se o elemento estiver nas bordas(ou seja, se 
estiver na coluna 0, ou numa linha=coluna, T(l, c)=1. 
Em qualquer uma das posicoes do meio, o valor de T será igual
a soma do valor logo acima, com o anterior a ele. Ou seja:
T(l, c) = T(l-1, c) + T(l-1, c-1)

Portanto:
    T(l,c) = 1, se l=c ou c=0
    T(l,c) = T(l-1, c)+T(l-1,c-1), nos demais casos.

Assim:
 |#
(define/memo (T linha coluna)
	(if (> coluna linha) (println "Nao existem triangulos assim, mas você pode travar seu computador."))
    (if   (or (= coluna 0) (= coluna linha))
          1
          (+ (T (- linha 1) coluna)
             (T (- linha 1) (- coluna 1)))))


			 