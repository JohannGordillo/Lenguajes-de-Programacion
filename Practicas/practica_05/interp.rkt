#lang plai

;; =======================================================================
;; >> Autores:
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
    [id (i)
        (interp (lookup i ds) (rest-env ds))]
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
    [fun (params body)(closure params body ds)]
    [app (fun args)
         (let* ([fun-val (interp fun ds)]
               [params (closure-param fun-val)]
               [init-env (closure-env fun-val)]
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


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE PRUEBAS.
;;
;; ///////////////////////////////////////////////////////////////////////


#| -----------------------------------------------------------------------
 | prueba: s-expression -> CFWAE-Value
 | -----------------------------------------------------------------------
 | Función para realizar pruebas al programa y verificar que el
 | resultado obtenido sea el esperado.
 | -----------------------------------------------------------------------
 |#
(define (prueba sexp)
  (interp (desugar (parse sexp)) (mtSub)))


(test (interp (desugar (parse '3)) (mtSub)) (numV 3))

(test (interp (desugar (parse #t)) (mtSub)) (boolV #t))

(test/exn (interp (desugar (parse 'x)) (mtSub))
          "lookup: Hay un identificador libre: x")

(test (interp (desugar (parse '{cond {#t 1} {#f 2} {else 3}})) (mtSub))
      (numV 1))

(test (interp (desugar (parse '{cond {#f 1} {#t 2} {else 3}})) (mtSub))
      (numV 2))

(test (interp (desugar (parse '{cond {#t 1} {#t 2} {else 3}})) (mtSub))
      (numV 1))

(test (interp (desugar (parse '{cond {#f 1} {#f 2} {else 3}})) (mtSub))
      (numV 3))

(test (interp (desugar (parse '(+ 1 1 1 (- 3 4 1) (sub1 1)))) (mtSub))
      (numV 1))

(test (prueba '{with {{a 2}} {sub1 a}}) (numV 1))

(test (prueba '(with* ((x 1) (y 2)) (+ y x))) (numV 3))

(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f 10}}}}
                       {+ x z}}) (numV 20))

(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f 1}}}}) (numV 4)) 

(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6)) 

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}})
          "lookup: Hay un identificador libre: x") 

(test (prueba '{{fun {x y} {+ x y}} 10 3}) (numV 13)) 

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y x} {+ x {+ y z}}}})
      (closure '(x y x)
               (op + (list (id 'x) (op + (list (id 'y) (id 'z)))))
               (aSub 'z (num 3)
                     (aSub 'y (num 2) (aSub 'x (num 1) (mtSub)))))) 

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f 3}}) (numV 6)) 

(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f 0}}) "lookup: Hay un identificador libre: x")

(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ 0 y}})
      (numV 4))

(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ x y}})
      (numV 5))

(test (prueba '{with {{x 5}} {+ x x}})
      (numV 10))

(test (prueba '{with {{x {+ 5 5}}} {+ x x}})
      (numV 20))

(test (prueba '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})
      (numV 14))

(test (prueba '{with {{x 5} {y {- 5 3}}} {+ x y}})
      (numV 7))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} 10}}})
      (numV 15))

(test (prueba '{with {{x 5}} {+ x {with {{y 3}} x}}})
      (numV 10))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} x}}})
      (numV 8))

(test (prueba '{with {{x 5}} {with {{y x}} y}})
      (numV 5))

(test (prueba '{with {{x 5}} {with {{x x}} x}})
      (numV 5))

(test (prueba '{{fun {x} x} 3}) (numV 3))

(test (prueba '{with {{x 3}} {fun {y} {+ x y}}})
      (closure '(y) (op + (list (id 'x) (id 'y)))
               (aSub 'x (num 3) (mtSub))))

(test/exn (prueba '{with {{x 5} {f {fun {y} {+ x y}}}} {f 10}})
          "lookup: Hay un identificador libre: x")

(test/exn (prueba '{or #t #f #t 1})
          "interp: Los argumentos no son de un mismo tipo de dato")

(test (prueba '{or #f #f #f #f}) (boolV #f))

(test (prueba '{or #f #f {or #t #f} #f}) (boolV #t))

(test (prueba '(not (or #f #f (or #t #f) #f))) (boolV #f))

(test (prueba '{and #t {or #t #f} #t}) (boolV #t))
