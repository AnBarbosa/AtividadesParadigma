#lang racket 

(require racket/format)
(define args (current-command-line-arguments))


(define arg0 (vector-ref args 0))
(define arg1 (vector-ref args 1))

(define (abre-arquivo-in arq) (open-input-file arq))
(define (abre-arquivo-out arq) (open-output-file arq #:exists 'append))


(define diretorio arg0)
(define arquivo-saida arg1)

(define (escreve t) (write t (abre-arquivo-out arquivo-saida)))

(define (nome-arquivo numero) (string-append  diretorio (number->string numero) ".txt"))
(define (le-lista numero)
    (read 
        (open-input-file 
            (nome-arquivo numero)
            #:mode 'text)))


(define (lista-das-listas lista num-atual lista-junta) 
    (cond [(file-exists? (nome-arquivo num-atual)) 
        (printf "Processando lista ~v\n" num-atual)
        (lista-das-listas lista (+ num-atual 1) (append (le-lista num-atual) lista-junta))]
        [else (printf "Fim") (sort lista-junta <)]))

(escreve (lista-das-listas '() 1 '()))