#lang plai

;; =======================================================================
;; >> Autor:
;;            Johann Gordillo [jgordillo@ciencias.unam.mx]
;;            Jhovan Gallardo [jhovan@ciencias.unam.mx]
;;
;; >> Fecha:
;;            11 / 05 / 2020
;; =======================================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2]
;;
;; Practica #05 - Implementación de CFWBAE.
;; =======================================================================

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

#|------------------------------------------------------------------------
 | desugar: SCFWBAE -> CFWBAE
 |------------------------------------------------------------------------
 | Función que toma una expresión con azúcar sintáctica
 | SCFWBAE y elimina el azúcar sintáctica, tansformándola
 | en una expresión del tipo CFWBAE; formando el árbol de
 | sintáxis abstracta correspondiente a la expresión recibida.
 |------------------------------------------------------------------------
 |#
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [iFS (condicion then else) (iF (desugar condicion)
                                   (desugar then)
                                   (desugar else))]
    [opS (f args) (op f (map desugar args))]
    [condS (cases) (desugar-cases cases)]
    [withS (bindings body)
           (app (fun (with-ids bindings) (desugar body))
                (map desugar (with-values bindings)))]
    [withS* (bindings body) (desugar (destar bindings body))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map desugar args))]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////


#|------------------------------------------------------------------------
 | desugar-cases: (listof Condition) -> CFWBAE
 |------------------------------------------------------------------------
 | Elimina el azúcar sintáctica de cada caso en una lista de
 | tipos de dato Condition.
 |------------------------------------------------------------------------
 |#
(define (desugar-cases cases)
  (type-case Condition (car cases)
    [condition (test-expr then-expr)
                 (iF (desugar test-expr)
                     (desugar then-expr)
                     (desugar-cases (cdr cases)))]
    [else-cond (else-expr)
               (desugar else-expr)]))

#|------------------------------------------------------------------------
 | with-ids: (listof Binding) -> (listof symbol)
 |------------------------------------------------------------------------
 | Devuelve una lista con los ids (symbols) de una lista de bindings,
 | la cual a su vez forma parte de un with.
 |------------------------------------------------------------------------
 |#
(define (with-ids bindings)
  (map (λ (b) (binding-id b)) bindings))


#|------------------------------------------------------------------------
 | with-values: (listof Binding) -> (listof SCFWBAE)
 |------------------------------------------------------------------------
 | Devuelve una lista con los values (SCFWBAEs) de una expresión with.
 |------------------------------------------------------------------------
 |#
(define (with-values bindings)
  (map (λ (b) (binding-value b)) bindings))


#| -----------------------------------------------------------------------
 | destar: (listof Binding) SCFWBAE -> SCFWBAE
 | -----------------------------------------------------------------------
 | Convierte un with* en with's anidados, para facilitar la eliminación
 | del azúcar sintáctica.
 | -----------------------------------------------------------------------
 |#
(define (destar bindings body)
  (cond
    [(empty? bindings) body]
    [else
     (withS (list (car bindings)) (destar (cdr bindings) body))]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE PRUEBAS.
;;
;; ///////////////////////////////////////////////////////////////////////


(test (desugar (parse '{with {{a 2}} {sub1 a}}))
      (app (fun '(a) (op sub1 (list (id 'a)))) (list (num 2))))

(test (desugar (parse '{with* {{x 1} {y 2}} {+ y x}}))
      (app (fun '(x)
                (app (fun '(y)
                          (op + (list (id 'y) (id 'x))))
                     (list (num 2))))
           (list (num 1))))
