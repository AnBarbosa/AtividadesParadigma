#lang racket

; Defina um procedimento que calcule 1/n. 
; O procedimento deverá aceitar apenas números e deverá executar
; apenas divisões válidas

#lang racket

(define (dividir-um-por n)
    (if (or (not (number? n)) (= n 0) ) 
        "Erro: não é número ou é zero"
        (/ 1 n)))
