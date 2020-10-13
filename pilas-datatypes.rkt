#lang eopl

;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743
;Gramática:
;; <Pila> :: ('Pila-Vacia)
;;        :: ('Pila-Valor scheme-value <Pila>)

;Definición del tipo de dato pila
(define-datatype stack-type stack?
  (empty-stack)
  (push
         [value always?];se utiliza el always para validar que es cualquier valor de scheme
         [saved-stack stack?]
  )
 )

;pop: Procedimiento que quita el primer elemento de una pila
(define pop
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () (eopl:error 'pop "No se puede eliminar un elemento de una lista vacía.")]
      [push (val saved-stack) saved-stack])))

;top: Procedimiento que retorna el primer elemento de una pila
(define top
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () (eopl:error 'pop "No se puede obtener el primer elemento de una lista vacía.")]
      [push (val saved-stack) val])))

;empty-stack?: Procedimiento que verifica si una pila está vacía
(define empty-stack?
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () #t]
      [push (val saved-stack) #f])))