#lang eopl
;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743

;Interfaz: Definición de funciones para sumas anidadas con representación utilizando datatypes

;suma-anidada ::= <valor> <numero>
;             ::=(<suma> suma-anidada suma-anidada)

;Definidión del tipo de dato suma-anidada
(define-datatype suma-anidada suma-anidada?
  (valor (numero number?))
  (suma
         [suma-a suma-anidada?]
         [suma-b suma-anidada?]
  )
 )

;Extractores

;valor-der: Procedimiento que saca el valor derecho de la suma anidada
(define valor-der
  (lambda (proc)
    (cases suma-anidada proc
      [valor (numero) numero]
      [suma (suma-a suma-b) suma-b])))

;valor-izq: Procedimiento que saca el valor izquierdo de la suma anidada
(define valor-izq
  (lambda (proc)
    (cases suma-anidada proc
      [valor (numero) numero]
      [suma (suma-a suma-b) suma-a])))

;sumar: Procedimiento que retorna el resultado de la suma anidada que se ingresa
(define sumar
  (lambda (proc)
    (cases suma-anidada proc
      [valor (numero) numero]
      [suma (suma-a suma-b)
            (+
             (sumar (valor-izq proc))
             (sumar (valor-der proc))
             )])))

;unparse: Procedimiento que dada una suma-anidada, la retorna en forma de lista
(define unparse
  (lambda (entrada)
    (cases suma-anidada entrada
      [valor (numero) numero]
      [suma (suma-a suma-b) (list 'suma
                                    (list 'valor(unparse (valor-izq entrada)))
                                    (list 'valor(unparse (valor-der entrada)))
                                  )
            ]
    )
  )
)

;parse: Procedimiento que dada una lista que cumpla con la gramática de suma-anidada, crea la estructura asociada
(define parse
  (lambda (entrada)
    (cond
      [(eqv? (car entrada) 'valor) (valor (cadr entrada))]
      [(eqv? (car entrada) 'suma)(suma
             (parse (cadr entrada))
              (parse (caddr entrada)))]
      [else #f]
     )
    )
  )