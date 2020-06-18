#lang plai

;; ==========================================================================
;; > Autores:
;;          Johann Gordillo [jgordillo@ciencias.unam.mx]
;;          Jhovan Gallardo [jhovan@ciencias.unam.mx]
;;
;; > Fecha:
;;          25 / 03 / 2020
;; ==========================================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2]
;;
;; Practica #03
;; ==========================================================================


(require (file "./grammars.rkt"))


#| --------------------------------------------------------------------------
 | parse: s-expression -> WAE
 | --------------------------------------------------------------------------
 | Toma una lista de números, symbolos o listas y la traduce a un árbol
 | de sintaxis abstracta WAE.
 |
 | Ejemplos:
 |
 | > (parse '{with {{a 2} {b 3}} {+ a b}})
 | (with (list (binding 'a (num 2)) (binding 'b (num 3)))
 |       (op #<procedure:+> (list (id 'a) (id 'b))))
 |
 | > (parse '{with {{x 2} {x 3}} x})
 | parse-bindings: Hay un identificador duplicado
 | --------------------------------------------------------------------------
 |#
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(empty? sexp) (error "Las expresiones vacias no son validas")]
    [(list? sexp)
     (case (first sexp)
       [(+ - * /)
        (op (select (car sexp)) (map parse (cdr sexp)))]
       [(modulo expt)
        (if (not (equal? (length sexp) 3))
            (error "Expresion no valida. Se requieren 2 operandos")
            (op
             (select-binary (car sexp))
             (list (parse (second sexp)) (parse (third sexp)))))]
       [(add1 sub1)
        (if (not (equal? (length sexp) 2))
            (error "Expresion no valida. Se requiere 1 operando.")
            (op
             (select-unary (car sexp))
             (list (parse (second sexp)))))]
       [(with)
        (with
         (parse-bindings (second sexp) #f)
         (parse (third sexp)))]
       [(with*)
        (with*
         (parse-bindings (second sexp) #t)
         (parse (third sexp)))])]))


;; //////////////////////////////////////////////////////////////////////////
;;
;; FUNCIONES AUXILIARES.
;;
;; //////////////////////////////////////////////////////////////////////////


#| --------------------------------------------------------------------------
 | parse-bindings
 | --------------------------------------------------------------------------
 | Parsea una lista de bindings, permitiendo o no duplicados dependiendo
 | del valor de verdad dado al argumento allow-duplicate.
 | --------------------------------------------------------------------------
 |#
(define (parse-bindings list-of-bindings allow-duplicate)
    (let ([comparator (lambda (x y) (symbol=? (first x) (first y)))])
      (if (or
           (boolean? (find-duplicates list-of-bindings comparator))
           allow-duplicate)
          (map
           (lambda (bnd) (binding (first bnd) (parse (cadr bnd))))
           list-of-bindings)
          (error 'parse-bindings "Hay un identificador duplicado"))))

#| --------------------------------------------------------------------------
 | find-duplicates
 | --------------------------------------------------------------------------
 | Halla duplicados en una lista, ayudandose de un comparador
 | pasado a la funcion como argumento.
 | --------------------------------------------------------------------------
 |#
(define (find-duplicates lst comparator) 
  (cond
    [(empty? lst) #f]
    [(member? (first lst) (cdr lst) comparator) (first lst)]
    [else (find-duplicates (cdr lst) comparator)]))

#| --------------------------------------------------------------------------
 | member?
 | --------------------------------------------------------------------------
 | Decide si un elemento esta en una lista.
 |
 | Si la lista es vacia, e no esta en ella y regresamos False.
 | Si la cabeza de la lista es igual a e, entonces e esta en la lista y
 | regresamos True. De otra manera, se hace recursion sobre la cola.
 | --------------------------------------------------------------------------
 |#
(define (member? e lst comparator)
  (cond
    [(empty? lst) #f]
    [(comparator (first lst) e) #t]
    [else (member? e (cdr lst) comparator)]))

#| --------------------------------------------------------------------------
 | select
 | --------------------------------------------------------------------------
 | Devuelve la operacion aritmetica seleccionada entre +, -, * y /.
 | --------------------------------------------------------------------------
 |#
(define (select op)
  (case op
    [(+) +]
    [(-) -]
    [(/) /]
    [(*) *]))

#| --------------------------------------------------------------------------
 | select-unary
 | --------------------------------------------------------------------------
 | Devuelve la operacion unaria seleccionada entre add1 y sub1.
 | --------------------------------------------------------------------------
 |#
(define (select-unary op)
  (case op
    [(add1) add1]
    [(sub1) sub1]))

#| --------------------------------------------------------------------------
 | select-binary
 | --------------------------------------------------------------------------
 | Devuelve la operacion binaria seleccionada entre modulo y expt.
 | --------------------------------------------------------------------------
 |#
(define (select-binary op)
  (case op
    [(modulo) modulo]
    [(expt) expt]))
