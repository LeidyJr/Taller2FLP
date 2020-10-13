#lang eopl
;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743
;Gramática:
;<NodeInSequence> ::= (<number> Listof(<number>) Listof(<number>))

;number->sequence: Procedimiento que dado un número, retorna una secuencia que consiste en ese número
(define number->sequence
  (lambda (num)
    (list num '() '())))

;current-element: Procedimiento que dada una secuencia, retorna el elemento actual (primer elemento)
(define current-element car)

;move-to-left: Procedimiento que dada una secuencia, pone como elemento actual el primer elemento de la lista izquierda, enviando el elemento actual a la lista derecha.
(define move-to-left
  (lambda (node)
    (let ([before (cadr node)])
      (if (null? before)
          (eopl:error 'move-to-left "Cannot move to left when at left end.")
          (let ([num (car node)]
                [after (caddr node)])
            (list (car before) (cdr before) (cons num after)))))))

;move-to-right: Procedimiento que dada una secuencia, pone como elemento actual el primer elemento de la lista derecha, enviando el elemento actual a la lista izquierda.
(define move-to-right
  (lambda (node)
    (let ([after (caddr node)])
      (if (null? after)
          (eopl:error 'move-to-right "Cannot move to right when at right end.")
          (let ([num (car node)]
                [before (cadr node)])
            (list (car after) (cons num before) (cdr after)))))))

;insert-to-left: Procedimiento que dados un numero y una secuencia, inserta el numero en la lista izquierda.
(define insert-to-left
  (lambda (num node)
    (let ([current (car node)]
          [before (cadr node)]
          [after (caddr node)])
      (list current (cons num before) after))))

;insert-to-left: Procedimiento que dados un numero y una secuencia, inserta el numero en la lista derecha.
(define insert-to-right
  (lambda (num node)
    (let ([current (car node)]
          [before (cadr node)]
          [after (caddr node)])
      (list current before (cons num after)))))

;at-left-end?: Procedimiento que verifica si ya no hay elementos en la lista izquierda, de ser así retorna true.
(define at-left-end?
  (lambda (node)
    (null? (cadr node))))

;at-right-end?: Procedimiento que verifica si ya no hay elementos en la lista derecha, de ser así retorna true.
(define at-right-end?
  (lambda (node)
    (null? (caddr node))))