#lang racket 

(require racket/format)
(require racket/trace)
(define args (current-command-line-arguments))


(define arg0 (string->number (vector-ref args 0)))
(define arg1 (string->number (vector-ref args 1)))
(define arg2 (string->number (vector-ref args 2)))

(define (abre-arquivo-in arq) (open-input-file arq))
(define (abre-arquivo-out arq) (open-output-file arq #:exists 'replace))


(define num-de-listas arg0)
(define valor-max arg1)
(define num-elementos arg2)

;
(define (random-list max size generated-list)
    (if 
        (= (length generated-list) size) 
        (sort generated-list <)
        (random-list max size (cons (random max) generated-list))))

(define (save-random-list name toSave max-value list-size)
    (cond [(= toSave 0) (println "FIM")]
        [else 
            (printf "Salvando lista ~v.\n" toSave)
            (write 
                (random-list max-value list-size '())
                (open-output-file 
                    (string-append name (number->string toSave) ".txt") 
                    #:exists 'replace))
            (save-random-list name (- toSave 1) max-value list-size)]))

;(trace save-random-list)
(save-random-list "" num-de-listas valor-max num-elementos)            