#lang eopl
;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743

;Gramática:
;; <Pila> :: ('Pila-Vacia)
;;        :: ('Pila-Valor scheme-value <Pila>)

;empty-stack: Procedimiento que retorna la representación de una pila vacía
(define empty-stack
  (lambda ()
    list 'Pila-Vacia))

;push: Procedimiento que agrega un valor v a una pila
(define push
  (lambda (v pila)
    (list 'Pila-Valor v pila)))
    
;pop: Procedimiento que quita el primer elemento de una pila
(define pop
  (lambda (pil)
    (if (empty-stack? pil)
        (eopl:error "No hay elementos para quitar")
        (if (eqv? (car pil) 'Pila-Valor)
            (cddr pil)
            (eopl:error "Pila mal formada")))
    ))
;top: Procedimiento que retorna el primer elemento de una pila
(define top
  (lambda (pila)
    (list (cadr pila))))
;empty-stack?: Procedimiento que verifica si una pila está vacía
(define empty-stack?
  (lambda (pila)
    (and (eqv? (car pila) 'Pila-Vacia)
             (null? (cdr pila)))
        ))