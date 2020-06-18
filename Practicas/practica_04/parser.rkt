#lang plai

;; =======================================================================
;; >> Autores:
;;            Johann Gordillo [jgordillo@ciencias.unam.mx]
;;            Jhovan Gallardo [jhovan@ciencias.unam.mx]
;;
;; >> Fecha:
;;            20 / 04 / 2020
;; =======================================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2]
;;
;; Practica #04 - Implementación de CFWAE.
;; =======================================================================


(require (file "./grammars.rkt"))


#| -----------------------------------------------------------------------
 | parse: s-expression -> CFWAE
 | -----------------------------------------------------------------------
 | Toma una lista de números, symbolos o listas y la traduce a un árbol
 | de sintaxis abstracta CFWAE.
 |
 | Ejemplos:
 |
 | > (parse '{fun {x y z} {+ x y 2}})
 | (fun '(x y z) (op #<procedure:+> (list (id 'x) (id 'y) (num 2))))
 |
 | > (parse '{with {{x 2} {x 3}} x})
 | Hay un identificador duplicado
 |
 | > (parse '{with {{a 1}} a})
 | (app (fun '(a) (id 'a)) (list (num 1)))
 | -----------------------------------------------------------------------
 |#
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(empty? sexp) (error "Las expresiones vacias no son validas")]
    [(list? sexp)
     (case (car sexp)
       [(+ - * /)
        (op (select (car sexp)) (map parse (cdr sexp)))]
       [(modulo expt)
        (if (not (equal? (length sexp) 3))
            (error 'parser "Error de aridad")
            (op (select-binary (car sexp)) (list (parse (second sexp)) (parse (third sexp)))))]
       [(add1 sub1)
        (if (not (equal? (length sexp) 2))
            (error 'parser "Error de aridad")
            (op (select-unary (car sexp)) (list (parse (second sexp)))))]
       [(if0)
        (if0 (parse (first (cdr sexp))) (parse (second (cdr sexp))) (parse (third (cdr sexp))))]
       [(with)
        (parse-with sexp)]
       [(with*)
        (with* (parse-bindings (second sexp)) (parse (third sexp)))]
       [(fun)
        (fun (second sexp) (parse (third sexp)))]
       [(app) (app (parse (second sexp)) (map (λ (arg) (parse arg)) (third sexp)))]
       [else (app (parse (first sexp)) (map (λ (arg) (parse arg)) (second sexp)))])]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////


#| -----------------------------------------------------------------------
 | parse-bindings:
 | (listof (pairof symbol s-expression)) boolean -> (listof Binding)
 | -----------------------------------------------------------------------
 | Parsea una lista de pares id-valor, permitiendo o no duplicados
 | dependiendo del valor de verdad dado al argumento allow-duplicate.
 | -----------------------------------------------------------------------
 |#
(define (parse-bindings list-of-bindings)
      (map (lambda (bnd) (binding (first bnd) (parse (cadr bnd))))
           list-of-bindings))


#| -----------------------------------------------------------------------
 | select: s-expression -> procedure
 | -----------------------------------------------------------------------
 | Devuelve la operacion aritmetica seleccionada entre +, -, * y /.
 | -----------------------------------------------------------------------
 |#
(define (select op)
  (case op
    [(+) +]
    [(-) -]
    [(/) /]
    [(*) *]))


#| -----------------------------------------------------------------------
 | select-unary: s-expression -> procedure
 | -----------------------------------------------------------------------
 | Devuelve la operacion unaria seleccionada entre add1 y sub1.
 | -----------------------------------------------------------------------
 |#
(define (select-unary op)
  (case op
    [(add1) add1]
    [(sub1) sub1]))


#| -----------------------------------------------------------------------
 | select-binary: s-expression -> procedure
 | -----------------------------------------------------------------------
 | Devuelve la operacion binaria seleccionada entre modulo y expt.
 | -----------------------------------------------------------------------
 |#
(define (select-binary op)
  (case op
    [(modulo) modulo]
    [(expt) expt]))


#| -----------------------------------------------------------------------
 | parse-with: s-expression -> CFWAE
 | -----------------------------------------------------------------------
 | Hace parse a una s-expression que contenga un with, quitando el
 | azúcar sintáctica con ayuda de la función desugar.
 | -----------------------------------------------------------------------
 |#
(define (parse-with sexp)
  (pseudo-desugar (parse-bindings (second sexp)) (parse (third sexp))))


#| -----------------------------------------------------------------------
 | pseudo-desugar: (listof Binding) CFWAE -> CFWAE
 | -----------------------------------------------------------------------
 | Recibe una lista de bindings y un cuerpo de un with, para regresar
 | su conversión a una aplicación de función.
 | -----------------------------------------------------------------------
 |#
(define (pseudo-desugar bindings body)
  (app (fun (with-ids bindings) body) (with-values bindings)))


#| -----------------------------------------------------------------------
 | with-ids: (listof Binding) -> (listof symbol)
 | -----------------------------------------------------------------------
 | Devuelve una lista con los ids (symbols) de una expresión with.
 | -----------------------------------------------------------------------
 |#
(define (with-ids bindings)
  (map (λ (b) (binding-name b)) bindings))


#| -----------------------------------------------------------------------
 | binding-id: Binding -> symbol
 | -----------------------------------------------------------------------
 | Devuelve el ID de un tipo de dato Binding.
 | -----------------------------------------------------------------------
 |#
(define (binding-name bnd)
  (type-case Binding bnd
    [binding (id value) id]))


#| -----------------------------------------------------------------------
 | with-values: (listof Binding) -> (listof CFWAE)
 | -----------------------------------------------------------------------
 | Devuelve una lista con los values (CFWAEs) de una expresión with.
 | -----------------------------------------------------------------------
 |#
(define (with-values bindings)
  (map (λ (b) (binding-val b)) bindings))


#| -----------------------------------------------------------------------
 | binding-value: Binding -> CFWAE
 | -----------------------------------------------------------------------
 | Devuelve el value de un tipo de dato Binding.
 | -----------------------------------------------------------------------
 |#
(define (binding-val bnd)
  (type-case Binding bnd
    [binding (id value) value]))

