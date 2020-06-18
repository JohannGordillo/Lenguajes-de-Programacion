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

#|------------------------------------------------------------------------
 | parse: s-expression -> SCFWBAE
 |------------------------------------------------------------------------
 | Toma una lista de números, símbolos o listas
 | y la traduce a un árbol de sintaxis abstracta SCFWBAE.
 |------------------------------------------------------------------------
 |#
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(empty? sexp)
     (error 'parser "Las expresiones vacías no son válidas")]
    [(list? sexp)
     (case (car sexp)
       [(true) (boolS #t)]
       [(false) (boolS #f)]
       [(+ - * / < <= = > >= and or)
        (opS (select-multiary (car sexp)) (map parse (cdr sexp)))]
       [(modulo expt)
        (if (not (equal? (length sexp) 3))
            (error 'parser "Error de aridad")
            (opS (select-binary (car sexp))
                 (list (parse (second sexp))
                       (parse (third sexp)))))]
       [(add1 sub1 not zero?)
        (if (not (equal? (length sexp) 2))
            (error 'parser "Error de aridad")
            (opS (select-unary (car sexp))
                 (list (parse (second sexp)))))]
       [(if)
        (iFS (parse (cadr sexp))
             (parse (caddr sexp))
             (parse (cadddr sexp)))]
       [(cond)
        (condS (map (λ p (parse-cond-pair (car p)))
                    (cdr sexp)))]
       [(with)
        (withS (parse-bindings (second sexp)) (parse (third sexp)))]
       [(with*)
        (withS* (parse-bindings (second sexp)) (parse (third sexp)))]
       [(fun)
        (let ([separador (second (cdr sexp))])
          (cond
            [(not (symbol=? separador ':)) (error 'parse (string-append "Separador incorrecto: " (symbol->string separador)))]
            [else (funS (parse-params (first (cdr sexp))) (parse-type (third (cdr sexp))) (parse (fourth (cdr sexp))))]))]
       [else
        (appS (parse (first sexp))
              (map (λ (arg) (parse arg)) (cdr sexp)))])]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////

#|------------------------------------------------------------------------
 | parse-type: s-expression -> Type
 |------------------------------------------------------------------------
 | Traduce en un árbol de síntaxis abstracta una s-expression
 | correspondiente a uno de los tipos posibles: number, boolean
 | y funciones.
 |------------------------------------------------------------------------
 |#
(define (parse-type sexp)
  (case sexp
    [(number) (numberT)]
    [(boolean) (booleanT)]
    [else
     (let ([n (- (length sexp) 2)])
       (funT (map parse-type (append (take sexp n) (list (last sexp))))))]))

#|------------------------------------------------------------------------
 | parse-params: (listof s-expression) -> (listof param)
 |------------------------------------------------------------------------
 | Dada una lista de expresiones, devuelve una lista con sus tipos
 | param asociados.
 |------------------------------------------------------------------------
 |#
(define (parse-params params)
  (map (λ (p) (let ([separador (second p)])
                (cond
                  [(not (symbol=? separador ':))
                   (error 'parse-params
                          (string-append
                           "Separador incorrecto: "
                           (symbol->string separador)))]
                  [else (param (first p) (parse-type (third p)))])))
       params))
  
#|------------------------------------------------------------------------
 | select: s-expression -> procedure
 |------------------------------------------------------------------------
 | Devuelve la operacion aritmetica seleccionada.
 | Las opciones posibles son las operaciones aritméticas +, *, -, /; y
 | los operadores de comparación <, <=, >, >= y =.
 |------------------------------------------------------------------------
 |#
(define (select-multiary op)
  (case op
    [(+) +]
    [(-) -]
    [(/) /]
    [(*) *]
    [(<) <]
    [(<=) <=]
    [(=) =]
    [(>) >]
    [(>=) >=]
    [(or) or]
    [(and) and]))

#|------------------------------------------------------------------------
 | select-unary: s-expression -> procedure
 |------------------------------------------------------------------------
 | Devuelve la operacion unaria seleccionada.
 | Las opciones posibles son add1, sub1, not y el predicado zero?.
 |------------------------------------------------------------------------
 |#
(define (select-unary op)
  (case op
    [(add1) add1]
    [(sub1) sub1]
    [(not) not]
    [(zero?) zero?]))

#|------------------------------------------------------------------------
 | select-binary: s-expression -> procedure
 |------------------------------------------------------------------------
 | Devuelve la operacion binaria seleccionada entre modulo y expt.
 | Las opciones posibles son modulo y expt.
 |------------------------------------------------------------------------
 |#
(define (select-binary op)
  (case op
    [(modulo) modulo]
    [(expt) expt]))

#|------------------------------------------------------------------------
 | parse-cond-pair: (pairof s-expression) -> Condition
 |------------------------------------------------------------------------
 | Dado un par con componentes de un condicional, lo transforma a
 | un tipo de dato Conditional.
 |------------------------------------------------------------------------
 |#
(define (parse-cond-pair cond-pair)
  (case (car cond-pair)
    [(else) (else-cond (parse (second cond-pair)))]
    [else (condition (parse (car cond-pair))
                     (parse (second cond-pair)))]))

#|------------------------------------------------------------------------
 | parse-bindings:
 | (listof (pairof symbol s-expression)) -> (listof Binding)
 |------------------------------------------------------------------------
 | Parsea una lista de pares id-valor, permitiendo o no duplicados
 | dependiendo del valor de verdad dado al argumento allow-duplicate.
 |------------------------------------------------------------------------
 |#
(define (parse-bindings list-of-bindings)
      (map (lambda (bnd)
             (binding (first bnd) (parse-type (third bnd)) (parse (fourth bnd))))
           list-of-bindings))

#|------------------------------------------------------------------------
 | or-pair: boolean boolean -> boolean 
 |------------------------------------------------------------------------
 | Realiza la operación booleana OR con un par de elementos.
 |------------------------------------------------------------------------
 |#
(define (or-pair b1 b2)
  (if b1 #t b2))

#|------------------------------------------------------------------------
 | or: (listof boolean) -> boolean 
 |------------------------------------------------------------------------
 | Redefinición de la operación booleana OR.
 |------------------------------------------------------------------------
 |#
(define (or xs)
  (foldr or-pair #f xs))

#|------------------------------------------------------------------------
 | and-pair: boolean boolean -> boolean 
 |------------------------------------------------------------------------
 | Realiza la operación booleana AND con un par de elementos.
 |------------------------------------------------------------------------
 |#
(define (and-pair b1 b2)
  (if b1 b2 #f))

#|------------------------------------------------------------------------
 | and: (listof boolean) -> boolean 
 |------------------------------------------------------------------------
 | Redefinición de la operación booleana AND.
 |------------------------------------------------------------------------
 |#
(define (and xs)
  (foldr and-pair #t xs))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE PRUEBAS UNITARIAS.
;;
;; ///////////////////////////////////////////////////////////////////////

;; Pruebas para ids.
(test (parse 'x) (idS 'x))

;; Pruebas para números.
(test (parse 1729) (numS 1729))

;; Pruebas para booleanos.
(test (parse true) (boolS #t))

(test (parse false) (boolS #f))

(test (parse #t) (boolS #t))

(test (parse #f) (boolS #f))

;; Prueba para expresiones vacías.
(test/exn (parse '{}) "parser: Las expresiones vacías no son válidas")

;; Pruebas para operaciones aritméticas.
(test (parse '{<= 1 2 3})
      (opS <= (list (numS 1) (numS 2) (numS 3))))

(test (parse '{modulo {+ 1 {- 2 5} {/ 3 2}} 1007})
      (opS modulo
           (list (opS + (list (numS 1)
                              (opS - (list (numS 2)
                                           (numS 5)))
                              (opS / (list (numS 3)
                                           (numS 2)))))
                 (numS 1007))))

(test (parse '{zero? {- 2 2}})
      (opS zero? (list (opS - (list (numS 2) (numS 2))))))

;; Pruebas para operaciones booleanas.
(test (parse '{and {or #t false} {and true #f}})
      (opS and (list (opS or (list (boolS #t)
                                   (idS 'false)))
                     (opS and (list (idS 'true)
                                    (boolS #f))))))

;; Pruebas para if.
(test (parse '{if {>= 1 2} 1729 {fun {}: number {+ 1 2}}})
      (iFS (opS >= (list (numS 1) (numS 2)))
           (numS 1729)
           (funS '() (numberT) (opS + (list (numS 1) (numS 2))))))

;; Pruebas para cond.
(test (parse '{cond {#t 1} {#f 2} {else 3}})
      (condS (list
              (condition (boolS #t) (numS 1))
              (condition (boolS #f) (numS 2))
              (else-cond (numS 3)))))

;; Pruebas para with.
(test (parse '{with {{a : number 2} {b : number 3}} {+ a b}})
      (withS (list (binding 'a (numberT) (numS 2))
                   (binding 'b (numberT) (numS 3)))
             (opS + (list (idS 'a) (idS 'b)))))


(test (parse '{with {{f : (number -> number)
                        {fun {{x : number}}: number x}}}
                    {f 1}})
      (withS (list (binding 'f
                            (funT (list (numberT) (numberT)))
                            (funS (list (param 'x (numberT)))
                                  (numberT)
                                  (idS 'x))))
             (appS (idS 'f) (list (numS 1)))))

;; Pruebas para with*.
(test (parse '{with* {{g : (number -> boolean)
                        {fun {{x : number}}: boolean {zero? x}}}}
                    {g 1729}})
      (withS* (list (binding 'g (funT (list (numberT) (booleanT)))
                             (funS (list (param 'x (numberT)))
                                   (booleanT)
                                   (opS zero? (list (idS 'x))))))
              (appS (idS 'g) (list (numS 1729)))))

(test (parse '{with* {{f : (number number -> boolean) {fun {{a : number} {b : number}}: boolean {zero? {- a b}}}}} {f 2 2}})
      (withS* (list (binding 'f
                             (funT (list (numberT) (numberT) (booleanT)))
                             (funS (list (param 'a (numberT))
                                         (param 'b (numberT)))
                                   (booleanT)
                                   (opS zero? (list (opS - (list (idS 'a) (idS 'b))))))))
              (appS (idS 'f) (list (numS 2) (numS 2)))))

;; Pruebas para fun.
(test (parse '{fun {{a : number} {b : number}}: number {+ a b}})
      (funS (list (param 'a (numberT)) (param 'b (numberT)))
            (numberT)
            (opS + (list (idS 'a) (idS 'b)))))

(test (parse '{fun {{x : number} {y : number} {z : number}}: number {+ x y z}})
      (funS (list (param 'x (numberT))
                  (param 'y (numberT))
                  (param 'z (numberT)))
            (numberT)
            (opS + (list (idS 'x) (idS 'y) (idS 'z)))))

;; Pruebas para aplicaciones de función.
(test (parse '{f x y z})
      (appS (idS 'f) (list (idS 'x) (idS 'y) (idS 'z))))
