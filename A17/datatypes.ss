
;; Parsed expression datatypes

; (define-datatype expression expression?
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;    (datum
;     (lambda (x)
;       (ormap 
;        (lambda (pred) (pred x))
;        (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp        ; applications
;    (rator expression?)
;    (rands (list-of expression?))]
;   [begin-exp
;   (exps (list-of expression?))]  
;   )
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure (params (list-of scheme-value?))
          (body  expression?)
          (env list?)]
  [informal-closure (params  scheme-value?)
          (body  expression?)
          (env list?)]
  [improper-closure
   (params (list-of symbol?))
   (rest symbol?)
   (bodies expression?)
   (env list?)
  ]
 )
	 
	 

(define scheme-value?
  (lambda (value)
    #t))
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))


