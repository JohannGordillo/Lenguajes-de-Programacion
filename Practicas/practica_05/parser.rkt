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

#|------------------------------------------------------------------------
 | parse: s-expression -> SCFWBAE
 |------------------------------------------------------------------------
 | Toma una lista de números, symbolos o listas
 | y la traduce a un árbol de sintaxis abstracta CFWBAE.
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
        (funS (second sexp) (parse (third sexp)))]
       [else
        (appS (parse (first sexp))
              (map (λ (arg) (parse arg)) (cdr sexp)))])]))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE FUNCIONES AUXILIARES.
;;
;; ///////////////////////////////////////////////////////////////////////


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
 | (listof (pairof symbol s-expression)) boolean -> (listof Binding)
 |------------------------------------------------------------------------
 | Parsea una lista de pares id-valor, permitiendo o no duplicados
 | dependiendo del valor de verdad dado al argumento allow-duplicate.
 |------------------------------------------------------------------------
 |#
(define (parse-bindings list-of-bindings)
      (map (lambda (bnd) (binding (first bnd) (parse (cadr bnd))))
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
;; SECCIÓN DE PRUEBAS.
;;
;; ///////////////////////////////////////////////////////////////////////


(test (parse 'x)
      (idS 'x))

(test (parse 1729)
      (numS 1729))

(test (parse #t)
      (boolS #t))

(test (parse #f)
      (boolS #f))

(test (parse true)
      (boolS #t))

(test (parse false)
      (boolS #f))

(test (parse '{+ x y 1729 {- {/ 1 2 3} {* 2 4 5}}})
      (opS +
           (list
            (idS 'x)
            (idS 'y)
            (numS 1729)
            (opS -
                 (list
                  (opS /
                       (list
                        (numS 1)
                        (numS 2)
                        (numS 3)))
                  (opS *
                       (list
                        (numS 2)
                        (numS 4)
                        (numS 5))))))))

(test (parse '{<= 1 2 3})
      (opS <= (list (numS 1) (numS 2) (numS 3))))
      
(test (parse '{fun {x y z} {+ x y z}})
      (funS '(x y z) (opS + (list (idS 'x) (idS 'y) (idS 'z)))))

(test (parse '{f x y z})
      (appS (idS 'f) (list (idS 'x) (idS 'y) (idS 'z))))

(test (parse '{cond {#t 1} {#f 2} {else 3}})
      (condS (list
              (condition (boolS #t) (numS 1))
              (condition (boolS #f) (numS 2))
              (else-cond (numS 3)))))

(test (parse '{if {>= 1 2} 1729 {fun {} {+ 1 2}}})
      (iFS (opS >= (list (numS 1) (numS 2)))
           (numS 1729)
           (funS '() (opS + (list (numS 1) (numS 2))))))
