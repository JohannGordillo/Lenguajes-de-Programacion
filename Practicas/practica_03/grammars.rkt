#lang plai

;; ==========================================================================
;; > Autores:
;;          Johann Gordillo [jgordillo@ciencias.unam.mx]
;;          Jhovan Gallardo [jhovan@ciencias.unam.mx]
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

#| --------------------------------------------------------------------------
 | Definición del tipo Binding
 | --------------------------------------------------------------------------
 |#
(define-type Binding
  [binding (id symbol?) (value WAE?)])

#| --------------------------------------------------------------------------
 | Definición del tipo AST
 | --------------------------------------------------------------------------
 |#
(define-type WAE
  [id    (i symbol?)]
  [num   (n number?)]
  [op    (f procedure?) (args (listof WAE?))]
  [with  (bindings (listof binding?)) (body WAE?)]
  [with* (bindings (listof binding?)) (body WAE?)])
