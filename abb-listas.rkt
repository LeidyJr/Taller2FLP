#lang eopl
;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743
;Gramática:
;<arbol-binario> := (arbol-vacio) empty
;                 := (nodo) <numero> <arbol-binario> <arbol-binario>

;Constructores
(define arbol-vacio (lambda () empty))
(define nodo (lambda (numero arbol-izq arbol-der) (list numero arbol-izq arbol-der)))

;Extractores
;arbol->hijo-izq: Procedimiento que dado un árbol, retorna su hijo izquierdo
(define arbol->hijo-izq
  (lambda (arbol)
    (car (cdr arbol))))
;arbol->hijo-der: Procedimiento que dado un árbol, retorna su hijo derecho
(define arbol->hijo-der
  (lambda (arbol)
    (car (cddr arbol))))
;nodo->valor Procedimiento que dado un nodo, retorna su valor
(define nodo->valor
  (lambda (arbol)
    (car arbol)))

;Predicados
;arbol-vacio?: Procedimiento que verifica si un árbol es vacío
(define arbol-vacio?(lambda (arbol) (equal? arbol (arbol-vacio))))
;arbol-hoja?: Procedimiento que verifica si un árbol es una hoja
(define arbol-hoja? (lambda (arbol) (and (not (arbol-vacio? arbol)) (equal? (arbol->hijo-izq arbol) (arbol-vacio)) (equal? (arbol->hijo-der arbol) (arbol-vacio)))))
;arbol-nodo?: Procedimiento que verifica si un árbol es un nodo
(define arbol-nodo? (lambda (arbol) (and (not (arbol-vacio? arbol)) (not (arbol-hoja? arbol)))))

;validador-orden: Función que recibe un árbol binario y verifica que los sub-árboles izquierdos tengan valores menores al nodo y los sub-árboles
;derechos tengan valores mayores al nodo.

(define validador-orden
  (lambda (n)
    (cond
      [(arbol-nodo? n )
       (cond
         [(and (not (arbol-vacio? (arbol->hijo-izq n))) (not (arbol-vacio? (arbol->hijo-der n))))
          (cond
            [(and (> (nodo->valor (arbol->hijo-der n)) (nodo->valor n)) (< (nodo->valor (arbol->hijo-izq n)) (nodo->valor n)))
             (and (validador-orden (arbol->hijo-izq n)) (validador-orden (arbol->hijo-der n)))
            ]
            [else #f]
          )
         ]
         [(not (arbol-vacio? (arbol->hijo-izq n)))
          (cond
            [(< (nodo->valor (arbol->hijo-izq n)) (nodo->valor n)) (validador-orden (arbol->hijo-izq n)) ]
            [else #f]
           )
         ]
         [(not (arbol-vacio? (arbol->hijo-der n)))
          (cond
            [(> (nodo->valor (arbol->hijo-der n)) (nodo->valor n)) (validador-orden (arbol->hijo-der n))]
            [else #f]
          )
         ]
       )
      ]
      [else #t]
    )
  )
)

;insertar-elemento: Función que recibe un árbol binario de búsqueda y un elemento n, si n ya existe, devuelve el árbol
;de lo contrario, inserta n en la posición indicada teniendo en cuenta el orden.

(define insertar-elemento
  (lambda (arbol n)
    (cond
      [(arbol-vacio? arbol) (nodo n (arbol-vacio) (arbol-vacio))]
      [(equal? (nodo->valor arbol) n) arbol]
      [(> n (nodo->valor arbol)) (nodo (nodo->valor arbol) (arbol->hijo-izq arbol) (insertar-elemento (arbol->hijo-der arbol) n))]
      [(< n (nodo->valor arbol)) (nodo (nodo->valor arbol) (insertar-elemento (arbol->hijo-izq arbol) n) (arbol->hijo-der arbol))]
    )
  )
)