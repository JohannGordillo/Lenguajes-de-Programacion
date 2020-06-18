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
(require (file "./interp.rkt"))
(require (file "./verificador.rkt"))

#|------------------------------------------------------------------------
 | execute: s-expression -> CFWBAE-Value
 |------------------------------------------------------------------------
 | Para el primer punto extra.
 |
 | Dada una s-expression, la parsea y posteriormente hace la verificación
 | de tipos. En caso de tener una tipificación correcta, elimina el
 | azúcar sintáctica de la expresión y finalmente la interpreta,
 | devolviendo un valor CFWBAE-Value.
 |------------------------------------------------------------------------
 |#
(define (execute sexp)
  (let ([exp (parse sexp)])
    (begin
      (typeof exp (phi))
      (interp (desugar exp) (mtSub)))))


;; ///////////////////////////////////////////////////////////////////////
;;
;; SECCIÓN DE PRUEBAS UNITARIAS.
;;
;; ///////////////////////////////////////////////////////////////////////

(test (execute '{with {{a : number 2}} {sub1 a}}) (numV 1))

(test (execute '(with* {{x : number 1} {y : number 2}} {+ y x})) (numV 3))

(test (execute '{with* {{x : number 5}
                        {w : number {+ x 1}}
                        {z : number {with {{x : number 10}
                                           {f : (number -> number) {fun {{a : number}}: number {+ x a}}}} {f 10}}}}
                       {+ x z}})
      (numV 20))

(test (execute '{with* {{x : number 3}}
                      {with* {{f : (number -> number) {fun {{y : number}}: number {+ x y}}}}
                             {with* {{x : number 4}}
                                    {f 1}}}}) (numV 4)) 

(test (execute '{with {{x : number 5} {y : number 1}} {+ x y}}) (numV 6))

(test (execute '{with* {{x : number 5} {y : number 1}} {+ x y}}) (numV 6)) 

(test (execute '{with* {{x : number 5} {y : number {+ x 1}}} {+ x y}}) (numV 11))

(test/exn (execute '{with {{x : number 5} {y : number {+ x 1}}} {+ x y}})
          "lookup: Hay un identificador libre: x") 

(test (execute '{{fun {{x : number} {y : number}}: number {+ x y}} 10 3}) (numV 13)) 

(test (execute '{with* {{x : number 1} {y : number 2} {z : number 3}} {fun {{x : number} {y : number} {x : number}}: number {+ x {+ y z}}}})
      (closure '(x y x)
               (op + (list (id 'x) (op + (list (id 'y) (id 'z)))))
               (aSub 'z (num 3)
                     (aSub 'y (num 2) (aSub 'x (num 1) (mtSub)))))) 

(test (execute '{with {{f : (number -> number) {fun {{x : number}}: number {+ x x}}}} {f 3}}) (numV 6)) 

(test/exn (execute '{with {{x : number 3} {f  : (number -> number) {fun {{a : number}}: number {+ x a}}}}
                      {f 0}}) "lookup: Hay un identificador libre: x")

(test (execute '{with* {{x : number 2} {y : number {+ x x}} {x : number 1}} {+ 0 y}})
      (numV 4))

(test (execute '{with* {{x : number 2} {y : number {+ x x}} {x : number 1}} {+ x y}})
      (numV 5))

(test (execute '{with {{x : number 5}} {+ x x}})
      (numV 10))

(test (execute '{with {{x : number {+ 5 5}}} {+ x x}})
      (numV 20))

(test (execute '{with {{x : number {+ 5 5}}} {with {{y : number {- x 3}}} {+ y y}}})
      (numV 14))

(test (execute '{with {{x : number 5} {y : number {- 5 3}}} {+ x y}})
      (numV 7))

(test (execute '{with {{x : number 5}} {+ x {with {{x : number 3}} 10}}})
      (numV 15))

(test (execute '{with {{x : number 5}} {+ x {with {{y : number 3}} x}}})
      (numV 10))

(test (execute '{with {{x : number 5}} {+ x {with {{x : number 3}} x}}})
      (numV 8))

(test (execute '{with {{x : number 5}} {with {{y : number x}} y}})
      (numV 5))

(test (execute '{with {{x : number 5}} {with {{x : number x}} x}})
      (numV 5))

(test (execute '{{fun {{x : number}}: number x} 3}) (numV 3))

(test (execute '{with {{x : number 3}} {fun {{y : number}}: number {+ x y}}})
      (closure '(y) (op + (list (id 'x) (id 'y)))
               (aSub 'x (num 3) (mtSub))))

(test/exn (execute '{with {{x : number 5}
                           {f : (number -> number) {fun {{y : number}}: number {+ x y}}}}
                          {f 10}})
          "lookup: Hay un identificador libre: x")

(test/exn (execute '{or #t #f #t 1})
          "typeof-op: Uno de los tipos no es boolean")

(test (execute '{or #f #f #f #f}) (boolV #f))

(test (execute '{or #f #f {or #t #f} #f}) (boolV #t))

(test (execute '(not (or #f #f (or #t #f) #f))) (boolV #f))

(test (execute '{and #t {or #t #f} #t}) (boolV #t))