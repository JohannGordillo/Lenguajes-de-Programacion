#lang plai

;; =======================================================================
;; > Autores:
;;          Johann Gordillo [jgordillo@ciencias.unam.mx]
;;          Jhovan Gallardo [jhovan@ciencias.unam.mx]
;;
;; > Fecha:
;;          09 / 03 / 2020
;; =======================================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2]
;;
;; Practica #02
;; =======================================================================


#| -----------------------------------------------------------------------
 | conjunto-cuadrado: (listof number) -> (listof (pairof number))
 | -----------------------------------------------------------------------
 | Funcion que recibe una lista y devuelve el conjunto
 | cuadrado de los elementos de dicha lista.
 | -----------------------------------------------------------------------
 |#
(define (conjunto-cuadrado lista)
  (remove-duplicates
   (foldr append '()
          (map
           (lambda (x)
             (map (lambda (y)
                    (list x y))
                  lista))
           lista))))

#| -----------------------------------------------------------------------
 | cambio: number number -> (number number number number number number)
 | -----------------------------------------------------------------------
 | Funcion que calcula el cambio que tenemos que devolver segun el
 | monto a cobrar y el monto pagado.
 |
 | Devuelve la cantidad de monedas de las denominaciones:
 | $50, $20, $10, $5, $2 y $1.
 | -----------------------------------------------------------------------
 |#
(define (cambio total pago) 0)  ;; TODO

#| -----------------------------------------------------------------------
 | descomposicion-primos: number -> (listof (pairof number))
 | -----------------------------------------------------------------------
 | Funcion que calcula la descomposicion en factores primos de un
 | numero.
 | -----------------------------------------------------------------------
 |#
(define (descomposicion-primos n) 0)  ;; TODO

#| -----------------------------------------------------------------------
 | multiplos: number number -> (listof number)
 | -----------------------------------------------------------------------
 | Funcion que recibe n, r y devuelve el conjunto de multiplos de n,
 | en el rango n y r.
 | -----------------------------------------------------------------------
 |#
(define (multiplos n r)
  (filter
        (lambda (x) (equal? (modulo x n) 0))  ;; Aplicamos esta funcion anonima
        (range n r)))                         ;; a cada elemento en un rango de n a r.

#| -----------------------------------------------------------------------
 | Figura
 | -----------------------------------------------------------------------
 | Tipo abstracto de datos para modelar figuras geometricas:
 | > Circulos.
 | > Cuadrados.
 | > Rectangulo.
 | > Triangulo.
 | -----------------------------------------------------------------------
 |#
(define-type Figura
  [Circulo (diametro number?)]
  [Cuadrado (lado number?)]
  [Rectangulo (base number?) (altura number?)]
  [Triangulo (base number?) (altura number?)])

#| -----------------------------------------------------------------------
 | perimetro: Figura -> number
 | -----------------------------------------------------------------------
 | Funcion que recibe una figura y calcula su perimetro.
 |
 | Observacion: Se supone que el triangulo es equilatero.
 | -----------------------------------------------------------------------
 |#
(define (perimetro figura)
  (type-case Figura figura
    [Circulo (diametro)
             (let ([radio (/ diametro 2)])
               (* 2 (* pi radio)))]
    [Cuadrado (lado) (* lado 4)]
    [Rectangulo (base altura) (+ (* 2 base) (* 2 altura))]
    [Triangulo (base altura) (* 3 base)]))

#| -----------------------------------------------------------------------
 | area: Figura -> number
 | -----------------------------------------------------------------------
 | Funcion que recibe una figura y calcula su area.
 | -----------------------------------------------------------------------
 |#
(define (area figura)
  (type-case Figura figura
    [Circulo (diametro)
             (let ([radio (/ diametro 2)])
              (* pi radio radio))]
    [Cuadrado (lado)
              (* lado lado)]
    [Rectangulo (base altura)
                (* base altura)]
    [Triangulo (base altura)
               (/ (* base altura) 2)]))

#| -----------------------------------------------------------------------
 | ABB - Arbol de Busqueda Binaria.
 | -----------------------------------------------------------------------
 | Tipo abstracto de datos para modelar arboles de busqueda binaria.
 |
 | Un arbol puede ser vacio, una hoja, o un nodo con dos ABB hijos:
 | uno derecho y uno izquierdo.
 | -----------------------------------------------------------------------
 |#
(define-type ABB
  [vacio]
  [hoja (elem number?)]
  [nodo (elem number?) (izq ABB?) (der ABB?)])

#| -----------------------------------------------------------------------
 | agrega: number ABB -> ABB
 | -----------------------------------------------------------------------
 | Funcion que recibe un numero n y un arbol binario, para agregar el
 | elemento al arbol de busqueda binario.
 | -----------------------------------------------------------------------
 |#
(define (agrega n arbol)
  (type-case ABB arbol
    [vacio () (hoja n)]
    [hoja (elem)
          (cond
                [(< elem n) (nodo elem (vacio) (hoja n))]
                [else (nodo elem (hoja n) (vacio))])]
    [nodo (elem a1 a2) (nodo elem a1 (agrega n a2))]))

#| -----------------------------------------------------------------------
 | altura: ABB -> number
 | -----------------------------------------------------------------------
 | Funcion que recibe un arbol binario y calcula su altura.
 | -----------------------------------------------------------------------
 |#
(define (altura arbol)
  (type-case ABB arbol
    [vacio () 0]
    [hoja (elem) 1]
    [nodo (elem a1 a2) (add1 (max (altura a1) (altura a2)))]))

#| -----------------------------------------------------------------------
 | contiene: ABB -> number -> boolean
 | -----------------------------------------------------------------------
 | Funcion que recibe un arbol binario, un elemento y
 | devuelve verdadero si el elemento esta contenido en el arbol,
 | falso en otro caso.
 | -----------------------------------------------------------------------
 |#
(define (contiene arbol e)
  (type-case ABB arbol
    [vacio () #f]
    [hoja (elem) (equal? elem e)]
    [nodo (elem a1 a2) (or
                        (equal? elem e)
                        (contiene a1 e)
                        (contiene a2 e))]))