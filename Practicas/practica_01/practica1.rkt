#lang plai

;; =========================================================
;; Autores:
;;         Johann Gordillo
;;         Jhovan Gallardo
;;
;; Fecha:
;;         01/02/2020
;; =========================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2] - Laboratorio
;; Practica 01 - Introducción a Racket
;; =========================================================


#|----------------------------------------------------------
 | area-cono: number number -> number
 |----------------------------------------------------------
 | Función que calcula el área de un cono de base circular.
 |
 | Argumentos:
 |     d - El diametro de la base.
 |     g - La generatriz de un cono circular recto.
 |----------------------------------------------------------
 |#
(define (area-cono d g)
  (let ([r (/ d 2)])
    (+ (* pi r g)
       (* pi (expt r 2)))))


#|----------------------------------------------------------
 | potencia: number number -> number
 |----------------------------------------------------------
 | Función que eleva un número a una potencia dada.
 |
 | Argumentos:
 |     a - El numero a elevar.
 |     b - La potencia a la cual elevar 'a'.
 |----------------------------------------------------------
 |#
(define (potencia a b)
  (cond
    [(and (zero? a) (zero? b))
     (error "La operacion no es valida")]
    [(zero? a) 0]
    [(zero? b) 1]
    [(> b 0) (* a (potencia a (sub1 b)))]
    [else (/ 1 (potencia a (* -1 b)))]))


#|----------------------------------------------------------
 | distancia: (pairof number) (pairof number) -> number
 |----------------------------------------------------------
 | Funcion que calcula la distancia entre dos puntos en
 | el plano.
 |
 | Argumentos:
 |     p - Las coordenadas (x y) del primer punto.
 |     q - Las coordenadas (x y) del segundo punto.
 |----------------------------------------------------------
 |#
(define (distancia p q)
  (let ([dx (- (car p) (car q))]
        [dy (- (cadr p) (cadr q))])
    (sqrt (+ (expt dx 2)
             (expt dy 2)))))


#|----------------------------------------------------------
 | neg?: number -> Boolean
 |----------------------------------------------------------
 | Función que nos dice si un numero es negativo o no.
 |
 | Argumentos:
 |     a - Un numero.
 |----------------------------------------------------------
 |#
(define (neg? a)
  (< a 0))


#|----------------------------------------------------------
 | absoluto: number -> number
 |----------------------------------------------------------
 | Funcion que nos regresa el valor absoluto de un numero.
 |
 | Argumentos:
 |     a - Un numero.
 |----------------------------------------------------------
 |#
(define (absoluto a)
  (if (neg? a) (* -1 a) a))


#|----------------------------------------------------------
 | divisor?: number number -> Boolean
 |----------------------------------------------------------
 | Predicado que nos dice si un numero es divisor de otro.
 |
 | Argumentos:
 |     n - Un numero
 |     m - Un numero
 |----------------------------------------------------------
 |#
(define (divisor? n m)
  (cond
   [(and (zero? n) (not (zero? m)))
    (error "El cero no divide numeros distintos de cero")]
   [(equal? n m) #t]
   [else (zero? (modulo m n))]))


#|----------------------------------------------------------
 | longitud: (listof any) -> number
 |----------------------------------------------------------
 | Función que nos da la longitud de una lista.
 |
 | Argumentos:
 |     lista - Una lista.
 |----------------------------------------------------------
 |#
(define (longitud lista)
  (cond
    [(empty? lista) 0]
    [(add1 (longitud (rest lista)))]))


#|----------------------------------------------------------
 | maximo: (listof number) -> number
 |----------------------------------------------------------
 | Función que nos da el elemento maximo de una lista.
 |
 | Argumentos:
 |     lista - Una lista de numeros.
 |----------------------------------------------------------
 |#
(define (maximo lista) (maximo_aux lista -inf.0))

(define (maximo_aux lista prev)
  (cond
    [(empty? lista) prev]
    [(> (car lista) prev) (maximo_aux (cdr lista) (car lista))]
    [else (maximo_aux (cdr lista) prev)]))


#|----------------------------------------------------------
 | reversa-lista: (listof any) -> (listof any)
 |----------------------------------------------------------
 | Función que nos da una lista invertida de la lista
 | pasada como parametro.
 |
 | Argumentos:
 |     lista - Una lista.
 |----------------------------------------------------------
 |#
(define (reversa-lista lista)
  (reversa-aux lista (sub1 (longitud lista))))

(define (reversa-aux lista n)
  (cond
    [(neg? n) empty]
    [else (cons (list-ref lista n) (reversa-aux lista (sub1 n)))]))


#|----------------------------------------------------------
 | palindromo?: (listof any) -> Boolean
 |----------------------------------------------------------
 | Predicado que nos dice si una lista contiene elementos
 | que forman un palindromo.
 |
 | Argumentos:
 |     lista - Una lista.
 |----------------------------------------------------------
 |#
(define (palindromo? lista)
  (equal? lista (reversa-lista lista)))


#|----------------------------------------------------------
 | divisores: number -> (listof number)
 |----------------------------------------------------------
 | Función que nos da una lista con los divisores de un
 | número dado.
 |
 | Argumentos:
 |     lista - Una lista.
 |----------------------------------------------------------
 |#
(define (divisores n)
  (cond
    [(< n 0) (divisores_aux (- n) (- n) '())]
    [(= n 0) '()]
    [else (divisores_aux n n '())]))

(define (divisores_aux n d lista)
  (cond
    [(= d 1) (cons 1 lista)]
    [(= (remainder n d) 0) (divisores_aux n (sub1 d) (cons d lista))]
    [else (divisores_aux n (sub1 d) lista)]))


;; ---------------------------------------------------------
;; Pruebas Unitarias.
;; ---------------------------------------------------------

(test (area-cono 4 3) 31.415)

(test (potencia 2 3) 8)

(test (distancia '(1 2) '(3 4)) 2.828)

(test (neg? -4) #t)

(test (absoluto -4) 4)

(test (divisor? 6 36) #t)

(test (longitud '(1 2 3 4 5)) 5)

(test (maximo '(1 5 4 3 2)) 5)

(test (reversa-lista '(1 2 3 4 5)) '(5 4 3 2 1))

(test (palindromo? '(1 2 3 4 3 2 1)) #t)

(test (divisores 15) '(1 3 5 15))
