
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
   (env environment?))
  (recursively-extended-env-record
    (vars (list-of symbol?))
    (vals (list-of scheme-value?))
    (bodies (list-of expression?))
    (env environment?)))


(define-datatype proc proc?
  [prim-proc
   (id symbol?)]
  [closure
   (vars scheme-value?)
   (body expression?)
   (env list?)]
   [acontinuation
	(cont continuation?)])
   

(define scheme-value?
  (lambda (value)
    #t))