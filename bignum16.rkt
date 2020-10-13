#lang eopl

;Presentado por:
;                Julian Fernando Anacona Beltrán - 2027790-3743
;                Leidy Johana Rivera Pazmiño - 2024011-3743
(define base 16)
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))

;Procedimiento que halla el sucesor de un número n, representado en cierta base.
(define successor
  (lambda (n)
    (if (is-zero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t base)
	      (cons 0 (successor (cdr n)))
	      (cons t (cdr n)))))))

;Procedimiento que halla el predecesor de un número n, representado en cierta base.
(define predecessor
  (lambda (n)
    (cond
     ((is-zero? n) #f)
     ((>= (car n) base) #f)
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  #f
	  (cons (- base 1) (predecessor (cdr n)))))
      (else (cons (- (car n) 1) (cdr n))))))

;Proceso que suma dos números dados en cierta base.
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

;Proceso que resta dos números dados en cierta base.
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;Proceso que multiplica mediante sumas dos números dados en cierta base.
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))

;Proceso que retorna la potencia de un número dado en cierta base.
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

;Proceso que retorna el factorial de un número dado en cierta base.
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))