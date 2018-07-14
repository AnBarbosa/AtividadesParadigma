#lang racket

; Defina um procedimento que tome três números como argumentos e
; retorne a soma dos quadrados dos dois maiores números
;

(define (maior-soma-dois-de-tres a b c) 
    (let (
         (lista (sort (list a b c) >)))
         (let (
               (maior1 (car lista))
               (maior2 (car (cdr lista)))               )
           (+ (* maior1 maior1) (* maior2 maior2)))))