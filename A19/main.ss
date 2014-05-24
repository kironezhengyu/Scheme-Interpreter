


(load "chez-init.ss")



(define load-all ; make it easy to reload the files
  (lambda ()
(load "datatypes.ss")
(load "parser.ss")
(load "cont.ss")
(load "env.ss")
(load "interpreter.ss")))

(load-all)

(define rep
  (lambda ()
    (display "-->")
    (set! init-cont (rep-cont))
    (top-level-eval (read) (end-cont (rep-cont)))))


(define eval-one-exp
  (lambda (exp)
    (set! init-cont (halt-cont))
    (top-level-eval exp (end-cont (halt-cont)))))

(define top-level-eval
  (lambda (form cont)
    (let ([parsed-form (parse-exp form)])
    (cases expression parsed-form
     [define-exp (sym val)
       (eval-exp (syntax-expand val) (extend-global-env-cont sym cont) (empty-env))]
     [else (eval-exp (syntax-expand parsed-form) cont (empty-env))])))) 