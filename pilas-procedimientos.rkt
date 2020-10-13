#lang eopl

;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743
;Gramática:
;; <Pila> :: ('Pila-Vacia)
;;        :: ('Pila-Valor scheme-value <Pila>)

;empty-stack: Procedimiento que retorna la representación de una pila vacía
(define (empty-stack)
  (list #t
        (lambda ()
          (eopl:error 'top "Pila vacía"))
        (lambda ()
          (eopl:error 'pop "Pila vacía"))))

;push: Procedimiento que agrega un elemento a una pila
(define (push element stack)
  (list #f
        (lambda () element)
        (lambda () stack)))

;empty-stack?: Procedimiento que verifica si una pila está vacía
(define (empty-stack? stack)
  (list-ref stack 0))

;top: Procedimiento que retorna el primer elemento de una pila
(define (top stack)
  ((list-ref stack 1)))

;pop: Procedimiento que quita el primer elemento de una pila
(define (pop stack)
  ((list-ref stack 2)))

