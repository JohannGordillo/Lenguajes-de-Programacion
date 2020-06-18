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

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value SCFWBAE?)])

;;Definición del tipo condition para la definición de cond.
(define-type Condition
  [condition (test-expr SCFWBAE?) (then-expr SCFWBAE?)]
  [else-cond (else-expr SCFWBAE?)])

;; Definición del tipo SCFWBAE
(define-type SCFWBAE
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (condicion SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS    (f procedure?) (args (listof SCFWBAE?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE?)]
  [funS   (params (listof symbol?)) (body SCFWBAE?)]
  [appS   (fun SCFWBAE?) (args (listof SCFWBAE?))])


;; Definición del tipo CFWBAE
(define-type CFWBAE
  [id   (i symbol?)]
  [num  (n number?)]
  [bool (b boolean?)]
  [iF   (condicion CFWBAE?) (then CFWBAE?) (else CFWBAE?)]
  [op   (f procedure?) (args (listof CFWBAE?))]
  [fun  (params (listof symbol?)) (body CFWBAE?)]
  [app  (fun CFWBAE?) (args (listof CFWBAE?))])

;; Data-type que representa un caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub  (name symbol?) (value CFWBAE?) (ds DefrdSub?)])

;;Data-type que representa la sintaxis abstracta de CFWBAE-Value
(define-type CFWBAE-Value
  [closure  (param (listof symbol?)) (body CFWBAE?) (env DefrdSub?)]
  [numV     (n number?)]
  [boolV    (b boolean?)])
