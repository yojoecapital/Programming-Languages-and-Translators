#lang racket

(require racket/trace)
(provide (all-defined-out)) ; export all top-level definitions for testing

;;;;; Part 1: Recursion and Lists

(define (rotate n lst)
    (let rec([i 0]
             [lst lst]
             [acc '()])
      (if (= i n)
          (append lst (reverse acc))
          (rec (+ i 1) (rest lst) (cons (first lst) acc)))))


(define (lookup key lst)
  (let rec([lst lst])
    (cond [(empty? lst) #f]
          [(equal? key (car (first lst))) (cdr (first lst))]
          [else (rec (rest lst))])))


(define (update key value lst)
  (let rec([lst lst]
           [acc '()])
    (cond [(empty? lst) (append acc `((,key . ,value)))]
          [(equal? key (car (first lst))) (append acc (append `((,key . ,value)) (rest lst)))]
          [else (rec (rest lst) (reverse (cons (first lst) acc)))])))


(define (equal-shape? shape1 shape2)
  (let rec([sh1 shape1]
           [sh2 shape2])
    (cond [(and (empty? sh1) (empty? sh2)) #t] ;they're both empty
          [(not (or (pair? sh1) (pair? sh2))) #t] ;they're both atoms
          [(and (pair? sh1) (pair? sh2)) ;they're both pairs
           (and (rec (cdr sh1) (cdr sh2)) (rec (car sh1) (car sh2)))] 
          [else #f])))


;;;;; Part 2: Higher Order Functions

(define (my-curry f . rest)
  (cond [(= (procedure-arity f) 0) f]
        [(= (procedure-arity f) (length rest))
         (apply f rest)]
        [else
         (let rec([args (reverse rest)]
                  [f f])
           (lambda next
                   (if (list? next)
                       (if (= (length (append next args)) (procedure-arity f)) ;if it is a list (racket more like "whack"et)
                            (apply f (reverse (append next args)))
                           (rec (append next args) f))
                       (if (= (length (cons next args)) (procedure-arity f)) ;if it is an atom (racket . (more . (like . ("whack" . (et . '())))))
                            (apply f (reverse (cons next args)))
                           (rec (cons next args) f)))))]))
                   
(define (my-map f lst)
    (let rec([f f]
             [lst lst]
             [acc '()])
      (if (empty? lst)
          acc
          (rec f (rest lst) (append acc `(,(f (first lst))))))))

(define (my-filter p lst)
  (let rec([p p]
           [lst lst]
           [acc '()])
    (cond [(empty? lst) acc]
          [else
           (if (p (first lst))
               (rec p (rest lst) (append acc `(,(first lst))))
               (rec p (rest lst) acc))])))

(define (make-object name)
  (let ([lst `((name . ,name))])
  (lambda (option . arg)
    (cond [(eq? option 'get) (lookup (first arg) lst)]
          [(eq? option 'set) (set! lst (update (first arg) (second arg) lst))]
          [(eq? option 'update) (set! lst (update (first arg) ((second arg) (lookup (first arg) lst)) lst))]))))


;;;;; Part 3: Meta-circular Evaluator

(define (my-eval rexp)
  (let my-eval-env ([rexp rexp]
                    [env '()])           ; environment (assoc list)
    (cond [(symbol? rexp)                ; variable
           (lookup rexp env)]
          [(eq? (first rexp) 'lambda)    ; lambda expression
           (lambda (x)
             (my-eval-env (third rexp) (cons (cons (first (second rexp)) x) env)))]
          [else                          ; function application
           (if (lookup (first rexp) env)
               ((lookup (first rexp) env) (my-eval-env (second rexp) env))
               ((my-eval-env (first rexp) env) (my-eval-env (second rexp) env)))])))


;;;;; Part 4: Free Variables

(define (free sexp)
  (let free-env ([sexp sexp]
                 [env '()])
    (cond [(symbol? sexp)
           (if (lookup sexp env)
               null
               (list sexp))]
          [(eq? (first sexp) 'lambda)
           (free-env (third sexp) (cons (cons (first (second sexp)) (first (second sexp))) env))]
          [else
           (if (lookup (first sexp) env)
               (free-env (second sexp) env)
               (append (free-env (first sexp) env) (free-env (second sexp) env)))])))