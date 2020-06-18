#lang plai

;; ==========================================================================
;; > Autores:
;;          Johann Gordillo [jgordillo@ciencias.unam.mx]
;;          Jhovan Gallardo [jhovan.gallardo@ciencias.unam.mx]
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
(require (file "./parser.rkt"))


#| --------------------------------------------------------------------------
 | subst: WAE symbol WAE -> WAE
 | --------------------------------------------------------------------------
 | Recibe una expresión (expr) del lenguaje WAE, un id (sub-id) y otra
 | expresión (value). Sustituye el valor sub-id por value, en expr.
 | --------------------------------------------------------------------------
 |#
(define (subst expr sub-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id) value expr)]
    [num (n) expr]
    [op (f args) (op f (map (λ (w) (subst w sub-id value)) args))]
    [with (bindings body)
          (let ([var-list (binding-variables bindings)])
            (let ([comparator (lambda (x y) (symbol=? x y))])
              (cond
                [(member? sub-id var-list comparator)
                 (with
                  (map (λ (b) (subst-value-binding b sub-id value)) bindings)
                  body)]
                [else
                 (with
                  (map (λ (b) (subst-value-binding b sub-id value)) bindings)
                  (subst body sub-id value))])))]
    [with* (bindings body)
          (let ([var-list (binding-variables bindings)])
            (let ([comparator (lambda (x y) (symbol=? x y))])
              (cond
                [(member? sub-id var-list comparator)
                 (with*
                  (map (λ (b) (subst-value-binding b sub-id value)) bindings)
                  body)]
                [else
                 (with*
                  (map (λ (b) (subst-value-binding b sub-id value)) bindings)
                  (subst body sub-id value))])))]))

#| --------------------------------------------------------------------------
 | interp: WAE -> number
 | --------------------------------------------------------------------------
 | Toma un árbol de sintáxis abstraca del lenguaje WAE y lo interpreta,
 | devolviendo el valor numérico correspondiente
 | --------------------------------------------------------------------------
 |#
(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp "Variable Libre")]
    [num (n) n]
    [op (f args) (apply f (map (λ (e) (interp e)) args))]
    [with
     (bindings body)
     (interp (apply-subst-bindings (interp-bindings bindings) body))]
    [with*
     (bindings body)
     (interp (apply-subst-bindings bindings body))]))


;; //////////////////////////////////////////////////////////////////////////
;;
;; FUNCIONES AUXILIARES.
;;
;; //////////////////////////////////////////////////////////////////////////


#| --------------------------------------------------------------------------
 | binding-variables
 | --------------------------------------------------------------------------
 | Devuelve una lista con los identificadores presentes
 | en la lista de bindings dada.
 | --------------------------------------------------------------------------
 |#
(define (binding-variables bindings)
  (map extract-id bindings))

#| --------------------------------------------------------------------------
 | extract-id
 | --------------------------------------------------------------------------
 | Devuelve la ID de un binding dado.
 | --------------------------------------------------------------------------
 |#
(define (extract-id bind)
  (type-case Binding bind
    [binding (id value) id]))

#| --------------------------------------------------------------------------
 | subst-value-binding
 | --------------------------------------------------------------------------
 | Hace la substitucion en el value de un binding.
 | --------------------------------------------------------------------------
 |#
(define (subst-value-binding bind sub-id val)
  (type-case Binding bind
    [binding (id value) (binding id (subst value sub-id val))]))

#| --------------------------------------------------------------------------
 | apply-subst-bind
 | --------------------------------------------------------------------------
 | Aplica substitucion a un binding individual.
 | --------------------------------------------------------------------------
 |#
(define (apply-subst-bind bind body)
  (type-case Binding bind
    [binding (id value) (subst body id value)]))

#| --------------------------------------------------------------------------
 | apply-subst-bindings
 | --------------------------------------------------------------------------
 | Aplica substitucion a una lista de bindings.
 | --------------------------------------------------------------------------
 |#
(define (apply-subst-bindings bind-list body)
  (cond
    [(empty? bind-list) body]
    [else
     (apply-subst-bind
      (car bind-list)
      (apply-subst-bindings (cdr bind-list) body))]))

#| --------------------------------------------------------------------------
 | interp-bindings
 | --------------------------------------------------------------------------
 | Aplica interp a una lista de bindings.
 | --------------------------------------------------------------------------
 |#
(define (interp-bindings bind-list)
  (cond
    [(empty? bind-list) '()]
    [else
     (cons
      (type-case Binding (car bind-list)
        [binding (id value) (binding id (num (interp value)))])
      (interp-bindings (cdr bind-list)))]))
