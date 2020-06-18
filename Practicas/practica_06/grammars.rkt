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

;;Data-type que define al tipo de dato Type
(define-type Type
  [numberT]
  [booleanT]
  [funT (params (listof Type?))])

;; Definición del tipo Type-Context
(define-type Type-Context
  [phi]
  [gamma (id symbol?)  (tipo Type?) (rest Type-Context?)])

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (tipo Type?) (value SCFWBAE?)])

;; Definición del tipo Param
(define-type Param
  [param (param symbol?) (tipo Type?)])

;;Definición del tipo condition para la definición de cond.
(define-type Condition
  [condition (test-expr SCFWBAE?) (then-expr SCFWBAE?)]
  [else-cond (else-expr SCFWBAE?)])

;; Definición del tipo SCFWBAEL
(define-type SCFWBAE
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (condicion SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS    (f procedure?) (args (listof SCFWBAE?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE?)]
  [funS   (params (listof param?)) (rType Type?) (body SCFWBAE?)]
  [appS   (fun SCFWBAE?) (args (listof SCFWBAE?))])


;; Gramáticas de la práctica anterior. Esto lo utilizarás sólo si
;; quieres ganar los puntos extra. Puedes eliminarlo, en otro caso.
;; Definición del tipo CFWBAE
(define-type CFWBAE
  [id   (i symbol?)]
  [num  (n number?)]
  [bool (b boolean?)]
  [iF   (condicion CFWBAE?) (then CFWBAE?) (else CFWBAE?)]
  [op   (f procedure?) (args (listof CFWBAE?))]
  [fun  (params (listof param?)) (body CFWBAE?)]
  [app  (fun CFWBAE?) (args (listof CFWBAE?))])

;; Data-type que representa un caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub  (name symbol?) (value CFWBAE?) (ds DefrdSub?)]);;-Value?) (ds DefrdSub?)]) ;; En la otra práctica esaba así

;;Data-type que representa la sintaxis abstracta de CFWBAE-Value
(define-type CFWBAE-Value
  [closure  (param (listof symbol?)) (body CFWBAE?) (env DefrdSub?)]
  [numV     (n number?)]
  [boolV    (b boolean?)])


