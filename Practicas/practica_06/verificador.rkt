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

#|------------------------------------------------------------------------
 | typeof: SCFWBAE Type-Context -> Type
 |------------------------------------------------------------------------
 | Toma un árbol de síntaxis abstracta CFWBAE y obtiene el tipo
 | de la expresión mínima.
 |------------------------------------------------------------------------
 |#
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS    (i)
            (lookup-context i context)]
    [numS   (n)
            (numberT)]
    [boolS  (b)
            (booleanT)]
    [iFS    (condicion then else)
            (typeof-if condicion then else context)]
    [opS    (f args)
            (typeof-op f args context)]
    [condS  (cases)
            (typeof-cond cases context)]
    [withS  (bindings body)
            (typeof body (build-bind-context bindings context))]
    [withS* (bindings body)
            (typeof body (build-bind-context bindings context))]
    [funS   (params rType body)
            (typeof-fun params rType body context)]
    [appS   (fun args)
            (typeof-app fun args context)]))

;; (prueba '{with* {{f : (number -> number) {fun {{x : number}}: number {+ x 1}}}} {f 1}})
;; (prueba '{cond {#t 1} {#f 2} {else 3}})


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////

#|------------------------------------------------------------------------
 | typeof-if
 |------------------------------------------------------------------------
 | Parsea una expresión ifS.
 |------------------------------------------------------------------------
 |#
(define (typeof-if condicion then else context)
  (let ([cond-type (typeof condicion context)]
                  [then-type (typeof then context)]
                  [else-type (typeof else context)])
              (cond
                [(booleanT? cond-type)
                 (cond
                   [(equal? then-type else-type) then-type]
                   [else
                    (error 'typeof-if
                     "El Then y el Else del iF no son del mismo tipo")])]
                [else
                 (error 'typeof-if
                  "La condición del iF no es de tipo boolean")])))

#|------------------------------------------------------------------------
 | typeof-fun
 |------------------------------------------------------------------------
 | Parsea una expresión funS.
 |------------------------------------------------------------------------
 |#
(define (typeof-fun params rType body context)
  (let ([new-context (build-param-context params context)])
    (if (equal? rType (typeof body new-context))
        (funT (append (map (λ (p) (param-tipo p)) params) (list rType)))
        (error 'typeof-fun
               "El valor de regreso no es del mismo tipo que el indicado"))))

#|------------------------------------------------------------------------
 | typeof-app
 |------------------------------------------------------------------------
 | Parsea una expresión appS.
 |------------------------------------------------------------------------
 |#
(define (typeof-app fun args context)
  (let* ([args-types (map (λ (a) (typeof a context)) args)]  ;; (listof Type)        -- Lista con los tipos de los argumentos.
         [fun-type (typeof fun context)]                     ;; (funT (listof Type)) -- Tipo de la función.
         [parametros (funT-params fun-type)]                 ;; (listof Type)        -- Se incluye el tipo de regreso.
         [return (first (reverse parametros))])              ;; (Type)               -- Tipo de regreso.
    (cond
      [(equal? (sub1 (length parametros)) (length args-types))  ;; Verificamos que el número de parámetros y argumentos sea el mismo.
       (if (equal-types-list? (take parametros (sub1 (length parametros))) args-types)  ;; Verificamos que las listas de tipos sean iguales.
           (if (idS? fun)  ;; En el caso de que la aplicación sea de la forma {f _}.
               return      ;; Se regresa simplemente el tipo de regreso.
               (if (equal? (typeof (funS-body fun) (build-param-context (funS-params fun) context)) return)  ;; Verificamos que el body regrese el tipo correcto.
                   return  ;; Si el body es de tipo correcto, regresamos este tipo.
                   (error 'typeof-app
                          "El valor de regreso no coincide con el resultado")))  ;; Si no, regresamos error.
           (error 'typeof-app
                  "El tipo de uno de los argumentos es incorrecto."))]   ;; Error en el caso de que las listas de tipos sean distintas.
      [else (error 'typeof-app
                   "El número de parámetros y argumentos es distinto")])))

#|------------------------------------------------------------------------
 | typeof-cond
 |------------------------------------------------------------------------
 | Parsea una expresión condS.
 |------------------------------------------------------------------------
 |#
(define (typeof-cond cases context)
  (let ([last-type "None"]) (if
                             (andmap (λ (c) (type-case Condition c
                                              [condition (test then)
                                                         (if (booleanT? (typeof test context))
                                                             (let ([then-type (typeof then context)])
                                                               (if (or (list (equal? last-type then-type) (equal? last-type "None")))
                                                                   #t
                                                                   (error 'typeof-cond
                                                                          "Una de las expresiones then tiene tipo distinto a las demás"))
                                                               (set! last-type then-type))
                                                             (error 'typeof-cond
                                                                    "Una de las expresiones test no es de tipo booleano"))]
                                              [else-cond (else-expr)
                                                         (if
                                                          (equal? last-type (typeof else-expr context))
                                                          #t
                                                          (error 'typeof-cond
                                                                 "La expresión else es de tipo distinto a las demás"))]))
                                     cases)
                             last-type
                             (error 'typeof-cond
                                    "Error de tipo en el condicional"))))

#|------------------------------------------------------------------------
 | typeof-op
 |------------------------------------------------------------------------
 | Parsea una expresión opS.
 |------------------------------------------------------------------------
 |#
(define (typeof-op f args context)
  (cond
    [(member? f (list + - * /))
     (if (andmap (λ (a) (numberT? (typeof a context))) args)
         (numberT)
         (error 'typeof-op
                "Uno de los tipos no es number"))]
    [(member? f (list < <= > >= =)) (if (andmap (λ (a) (numberT? (typeof a context))) args)
                                        (booleanT)
                                        (error 'typeof-op
                                               "Uno de los tipos no es number"))]
    [(member? f (list and or)) (if (andmap (λ (a) (booleanT? (typeof a context))) args)
                                   (booleanT)
                                   (error 'typeof-op
                                          "Uno de los tipos no es boolean"))]
    [(member? f (list modulo expt)) (if (not (equal? (length args) 2))
                                        (error 'typeof (string-append
                                                        "La operación debe recibir 2 operandos"))
                                        (if (andmap (λ (a) (numberT? (typeof a context))) args)
                                            (numberT)
                                            (error 'typeof-op
                                                   "Uno de los tipos no es number")))]
    [(member? f (list add1 sub1)) (if (not (equal? (length args) 1))
                                      (error 'typeof-op
                                             "La operación debe recibir 1 operando")
                                      (if (andmap (λ (a) (numberT? (typeof a context))) args)
                                          (numberT)
                                          (error 'typeof "Uno de los tipos no es number")))]
    [(equal? f zero?) (if (not (equal? (length args) 1))
                          (error 'typeof-op
                                 "La operación debe recibir 1 operando")
                          (if (andmap (λ (a) (numberT? (typeof a context))) args)
                              (booleanT)
                              (error 'typeof-op
                                     "Uno de los tipos no es number en la operación " (symbol->string f))))]
    [(equal? f not) (if (not (equal? (length args) 1))
                        (error 'typeof-op
                               "La operación debe recibir 1 operando")
                        (if (andmap (λ (a) (booleanT? (typeof a context))) args)
                            (booleanT)
                            (error 'typeof-op
                                   "El operando no es de tipo boolean")))]))

#|------------------------------------------------------------------------
 | lookup-context: symbol Context -> Type
 |------------------------------------------------------------------------
 | Busca una variable en el contexto. En caso de hallarla, devuelve
 | el tipo de la misma. De otra manera, da error.
 |------------------------------------------------------------------------
 |#
(define (lookup-context id context)
  (type-case Type-Context context
    [phi () (error 'lookup-context (string-append
                   "Hay un identificador libre: "
                   (symbol->string id)))]
    [gamma (i tipo rest)
           (if (symbol=? id i)
               tipo
               (lookup-context id rest))]))

#|------------------------------------------------------------------------
 | equal-types-list?: (listof Type) (listof Type) -> boolean
 |------------------------------------------------------------------------
 | Devuelve True si las listas tienen los mismos tipos. False en otro
 | caso.
 |------------------------------------------------------------------------
 |#
(define (equal-types-list? params args)
  (cond
    [(empty? params) #t]
    [(equal? (car params) (car args))
     (equal-types-list? (cdr params) (cdr args))]
    [else #f]))

#|------------------------------------------------------------------------
 | build-param-context: (listof Param) Context -> Context
 |------------------------------------------------------------------------
 | Contruye un contexto a partir de un contexto inicial y una lista
 | de parámetros.
 |------------------------------------------------------------------------
 |#
(define (build-param-context params init-context)
  (cond
    [(empty? params) init-context]
    [else
     (let ([par (car params)])
       (type-case Param par
         [param (p type)
                (build-param-context (cdr params) (gamma p type init-context))]))]))

#|------------------------------------------------------------------------
 | build-bind-context: (listof Binding) Context -> Context
 |------------------------------------------------------------------------
 | Contruye un contexto a partir de un contexto inicial y una lista
 | de bindings.
 |------------------------------------------------------------------------
 |#
(define (build-bind-context bindings init-context)
  (cond
    [(empty? bindings) init-context]
    [else
     (let ([bnd (car bindings)])
       (type-case Binding bnd
         [binding (id type value)
                  (if (not (equal? (typeof value init-context) type))
                      (error 'build-bind-context "Error de tipos")
                      (build-bind-context (cdr bindings) (gamma id type init-context)))]))]))

#|------------------------------------------------------------------------
 | member?: any (listof any) -> boolean
 |------------------------------------------------------------------------
 | Nos dice si un elemento es miembro de una lista.
 |------------------------------------------------------------------------
 |#
(define (member? x xs)
  (cond
    [(empty? xs) #f]
    [(equal? x (car xs)) #t]
    [else (member? x (cdr xs))]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE PRUEBAS UNITARIAS.
;;
;; ///////////////////////////////////////////////////////////////////////

(define (prueba expr)
  (typeof (parse expr) (phi)))
