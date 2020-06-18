#lang plai

;; =======================================================================
;; >> Autores:
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
(require (file "./desugar.rkt"))

#|------------------------------------------------------------------------
 | lookup: symbol DefrdSub -> CFWBAE
 |------------------------------------------------------------------------
 | Busca el identificador "name" en el caché de sustitución "ds"
 | regresando el valor correspondiente o informando un error si
 | no lo encuentra.
 |------------------------------------------------------------------------
 |#
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()
           (error 'lookup
                  (string-append
                   "Hay un identificador libre: "
                   (symbol->string name)))]
    [aSub (name-ds value-ds rest-ds)
          (if (symbol=? name-ds name)
              value-ds
              (lookup name rest-ds))]))

#|------------------------------------------------------------------------
 | interp: CFWBAE DefrdSub -> CFWBAE-Value
 |------------------------------------------------------------------------
 | Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
 | sustituciones y lo interpreta dependiendo de las definiciones dentro
 | del caché, devolviendo el valor numérico correspondiente.
 |------------------------------------------------------------------------
 |#
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i) (interp (lookup i ds) (rest-env ds))]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [iF (condicion then else)
        (if (boolV-b (interp condicion ds))
            (interp then ds)
            (interp else ds))]
    [op (f args) (let* ([lista
                         (map (λ (e) (let ([evl (interp e ds)])
                                       (if (numV? evl)
                                           (numV-n evl)
                                           (boolV-b evl))))
                              args)]
                        [res (cond
                               [(list-of-numbers? lista)
                                (apply f lista)]
                               [(list-of-booleans? lista)
                                (f lista)]
                               [else (error 'interp
                                     "Los argumentos no son de un mismo tipo de dato")])])
                   (if (number? res)
                       (numV res)
                       (boolV res)))]
    [fun (params body) (closure (map (λ (p) (param-param p)) params) body ds)]
    [app (fun args)
         (let* ([fun-val (interp fun ds)]
               [params (closure-param fun-val)]
               [init-env (closure-env fun-val)]
               [argumentos (map (λ (a) (interp a init-env)) args)]
               [fenv (final-env params args init-env)])
           (interp (closure-body fun-val) fenv))]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////

#| -----------------------------------------------------------------------
 | is-list-of-numbers?
 | -----------------------------------------------------------------------
 | Devuelve true si la lista está compuesta únicamente por valores
 | numericos. Si no es así, devuelve false.
 | -----------------------------------------------------------------------
 |#
(define (list-of-numbers? xs)
  (cond
    [(empty? xs) #t]
    [(number? (car xs)) (list-of-numbers? (cdr xs))]
    [else #f]))

#| -----------------------------------------------------------------------
 | is-list-of-booleans?
 | -----------------------------------------------------------------------
 | Devuelve true si la lista está compuesta únicamente por valores
 | booleanos. Si no es así, devuelve false.
 | -----------------------------------------------------------------------
 |#
(define (list-of-booleans? xs)
  (cond
    [(empty? xs) #t]
    [(boolean? (car xs)) (list-of-booleans? (cdr xs))]
    [else #f]))


#| -----------------------------------------------------------------------
 | final-env: (listof symbol) (listof CFWAE) -> DefrdSub
 | -----------------------------------------------------------------------
 | Construye un ambiente de evaluación a partir de parametros
 | y argumentos.
 |
 | Se considera que el número de params es igual al de args.
 | -----------------------------------------------------------------------
 |#
(define (final-env params args env)
  (cond
    [(empty? params) env]
    [else
     (aSub (car params) (car args) (final-env (cdr params) (cdr args) env))]))


#| -----------------------------------------------------------------------
 | rest-env: DefrdSub -> DefrdSub
 | -----------------------------------------------------------------------
 | Devuelve el resto de un ambiente dado.
 | -----------------------------------------------------------------------
 |#
(define (rest-env ds)
  (type-case DefrdSub ds
    [mtSub () mtSub]
    [aSub (name value rest) rest]))
