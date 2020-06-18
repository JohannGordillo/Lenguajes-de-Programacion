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
(require (file "./parser.rkt"))


#| -----------------------------------------------------------------------
 | lookup: symbol DefrdSub -> CFWAE
 | -----------------------------------------------------------------------
 | Busca el identificador "name" en el caché de sustitución "ds"
 | regresando el valor correspondiente o informando un error si no
 | lo encuentra.
 | -----------------------------------------------------------------------
 |#
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()
           (error 'lookup (string-append "Hay un identificador libre: " (symbol->string name)))]
    [aSub (name-ds value-ds rest-ds)
          (if (symbol=? name-ds name)
              value-ds
              (lookup name rest-ds))]))


#| -----------------------------------------------------------------------
 | interp: CFWAE DefrdSub-> CFWAE-Value
 | -----------------------------------------------------------------------
 | Toma un árbol de sintáxis abstracta del lenguaje CFWAE, un caché de
 | sustituciones y lo interpreta dependiendo de las definiciones 
 | dentro del caché, devolviendo el valor numérico correspondiente.
 | -----------------------------------------------------------------------
 |#
(define (interp expr ds)
  (type-case CFWAE expr
    [id (i)
        (interp (lookup i ds) (rest-env ds))]
    [num (n)
        (numV n)]
    [op (f args)
        (numV (apply f (map (λ (e) (numV-n (interp e ds))) args)))]
    [if0 (cond then else)
         (let ([cond-value (interp cond ds)])
           (if (numV? cond-value)
               (if (zero-value? cond-value)
                   (interp then ds)
                   (interp else ds))
               (error 'interp "Símbolo no esperado la condicional de if0, no es un número")))]
    [with* (bindings body)
           (interp (desugar expr) ds)]
    [fun (params body)
         (closure params body ds)]
    [app (fun args)
         (let* ([fun-val (interp fun ds)]
               [params (closure-param fun-val)]
               [init-env (closure-env fun-val)]
               [fenv (final-env params args init-env)])
           (interp (closure-body fun-val) fenv))]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////


#| -----------------------------------------------------------------------
 | numV-n: numV -> number
 | -----------------------------------------------------------------------
 | Regresa el entero asociado a un numV dado.
 | -----------------------------------------------------------------------
 |#
(define (numV-n num)
  (type-case CFWAE-Value num
    [numV (n)
          n]
    [else num]))


#| -----------------------------------------------------------------------
 | binding-variables: (listof Binding) -> (listof symbol)
 | -----------------------------------------------------------------------
 | Devuelve una lista con los identificadores presentes
 | en la lista de bindings dada.
 | -----------------------------------------------------------------------
 |#
(define (binding-variables bindings)
  (map extract-id bindings))


#| -----------------------------------------------------------------------
 | extract-id: Binding -> symbol
 | -----------------------------------------------------------------------
 | Devuelve la ID de un binding dado.
 | -----------------------------------------------------------------------
 |#
(define (extract-id bind)
  (type-case Binding bind
    [binding (id value) id]))


#| -----------------------------------------------------------------------
 | binding-values: (listof Binding) -> (listof CFWAE)
 | -----------------------------------------------------------------------
 | Devuelve una lista con los values presentes
 | en la lista de bindings dada.
 | -----------------------------------------------------------------------
 |#
(define (binding-values bindings)
  (map extract-value bindings))


#| -----------------------------------------------------------------------
 | extract-value: Binding -> CFWAE
 | -----------------------------------------------------------------------
 | Devuelve el value de un binding dado.
 | -----------------------------------------------------------------------
 |#
(define (extract-value bind)
  (type-case Binding bind
    [binding (id value) value]))


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


#| -----------------------------------------------------------------------
 | desugar: CFWAE -> CFWAE
 | -----------------------------------------------------------------------
 | Elimina el azucar sintactica de una expresion CFWAE dada.
 | -----------------------------------------------------------------------
 |#
(define (desugar expr)
  (type-case CFWAE expr
    [id (i) (id i)]
    [num (n) (num n)]
    [op (f args) (op f (map (λ (e) (desugar e)) args))]
    [if0 (cond then else) (if0 (desugar cond) (desugar then) (desugar else))]
    [fun (params body) (fun params (desugar body))]
    [app (fun args) (app (desugar fun) (map (λ (a) (desugar a)) args))]
    [with* (bindings body)
           (cond
             [(empty? bindings) (desugar body)]
             [else
              (let ([var (car (binding-variables bindings))]
                    [value (car (binding-values bindings))])
                (desugar (app (fun (list var) (with* (cdr bindings) body)) (list value))))])]))


#| -----------------------------------------------------------------------
 | zero-value?: CFWAE-Value -> boolean
 | -----------------------------------------------------------------------
 | Verifica si el entero asociado a un numV es cero.
 | -----------------------------------------------------------------------
 |#
(define (zero-value? numv)
  (type-case CFWAE-Value numv
    [numV (n) (zero? n)]
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
 | closure-body: CFWAE-Value -> CFWAE
 | -----------------------------------------------------------------------
 | Devuelve el body de un closure dado.
 | -----------------------------------------------------------------------
 |#
(define (closure-body cls)
  (type-case CFWAE-Value cls
    [closure (param body env)
          body]
    [else (error 'closure-body "No es un closure.")]))


#| -----------------------------------------------------------------------
 | closure-param: CFWAE-Value -> (listof symbol)
 | -----------------------------------------------------------------------
 | Devuelve el parametro de un closure.
 | -----------------------------------------------------------------------
 |#
(define (closure-param cls)
  (type-case CFWAE-Value cls
    [closure (param body env)
          param]
    [else (error 'closure-body "No es un closure.")]))


#| -----------------------------------------------------------------------
 | closure-env: CFWAE-Value -> DefrdSub
 | -----------------------------------------------------------------------
 | Devuelve el ambiente de un closure.
 | -----------------------------------------------------------------------
 |#
(define (closure-env cls)
  (type-case CFWAE-Value cls
    [closure (param body env)
          env]
    [else (error 'closure-body "No es un closure.")]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; PRUEBAS.
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
  (interp (parse sexp) (mtSub)))

(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f {10}}}}}
                       {+ x z}}) (numV 20))


(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4)) 

(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6)) 

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x") 

(test (prueba '{app {fun {x y} {+ x y}} {10 3}}) (numV 13)) 

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y x} {+ x {+ y z}}}})
      (closure '(x y x) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (num 3) (aSub 'y (num 2) (aSub 'x (num 1) (mtSub)))))) 

(test (prueba '{if0 {- 1 1} 5 6}) (numV 5))

(test (prueba '{if0 {+ 1 2} 5 6}) (numV 6)) 

(test/exn (prueba '{if0 {fun {x} {+ x 1}} 5 6}) "interp: Símbolo no esperado la condicional de if0, no es un número")

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6)) 

(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")

(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ 0 y}}) (numV 4))

(test (prueba '{with* {{x 2} {y {+ x x}} {x 1}} {+ x y}}) (numV 5))

(test (prueba '{with {{x 5}} {+ x x}}) (numV 10))

(test (prueba '{with {{x {+ 5 5}}} {+ x x}}) (numV 20))

(test (prueba '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}}) (numV 14))

(test (prueba '{with {{x 5} {y {- 5 3}}} {+ x y}}) (numV 7))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} 10}}}) (numV 15))

(test (prueba '{with {{x 5}} {+ x {with {{y 3}} x}}}) (numV 10))

(test (prueba '{with {{x 5}} {+ x {with {{x 3}} x}}}) (numV 8))

(test (prueba '{with {{x 5}} {with {{y x}} y}}) (numV 5))

(test (prueba '{with {{x 5}} {with {{x x}} x}}) (numV 5))

(test (prueba '{{fun {x} x} {3}}) (numV 3))

(test (prueba '{with {{x 3}} {fun {y} {+ x y}}})
      (closure '(y) (op + (list (id 'x) (id 'y))) (aSub 'x (num 3) (mtSub))))

(test/exn (prueba '{with {{x 5} {f {fun {y} {+ x y}}}} {f {10}}}) "lookup: Hay un identificador libre: x")
