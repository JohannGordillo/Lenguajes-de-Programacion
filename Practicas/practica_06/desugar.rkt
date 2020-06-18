#lang plai

;; =======================================================================
;; >> Autor:
;;            Johann Gordillo [jgordillo@ciencias.unam.mx]
;;            Jhovan Gallardo [jhovan@ciencias.unam.mx]
;;
;; >> Fecha:
;;            29 / 05 / 2020
;; =======================================================================
;; Universidad Nacional Autonoma de Mexico
;; Facultad de Ciencias
;;
;; Lenguajes de Programacion [2020-2]
;;
;; Practica #06 - Verificación de tipos.
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
    [idS    (i) (id i)]
    [numS   (n) (num n)]
    [boolS  (b) (bool b)]
    [iFS    (condicion then else) (iF (desugar condicion)
                                   (desugar then)
                                   (desugar else))]
    [opS    (f args) (op f (map desugar args))]
    [condS  (cases) (desugar-cases cases)]
    [withS  (bindings body)
            (app (fun (bindings-to-params bindings) (desugar body))
                (map desugar (with-values bindings)))]
    [withS* (bindings body) (desugar (destar bindings body))]
    [funS   (params rType body) (fun params (desugar body))]
    ;;[funS   (params rType body) (fun params rType (desugar body))]
    [appS   (fun args) (app (desugar fun) (map desugar args))]))


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
 | bindings-to-params: (listof Binding) -> (listof Param)
 |------------------------------------------------------------------------
 | Convierte una lista de Bindings en una lista de Params.
 |------------------------------------------------------------------------
 |#
(define (bindings-to-params bindings)
  (map (λ (bnd) (param (binding-id bnd) (binding-tipo bnd))) bindings))

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
