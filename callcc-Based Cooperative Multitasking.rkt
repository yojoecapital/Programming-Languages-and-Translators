#lang racket

(define q '())

(define (spawn)
  (println "<SPAWN>")
  (let ([k (call/cc (lambda (c) c))])
    (if (procedure? k)
        (set! q (reverse (cons k q)))
        #f)))

(define (yield)
  (println "<CST>")
  (cond [(not (empty? q))
         (let ([k (call/cc (lambda (c) c))])
           (cond [(procedure? k)
               (let ([next (first q)]
                     [remain (rest q)])
                 (set! q (reverse (cons k remain)))
                 (next 0))]))]))


(define (terminate)
  (println "<TERM>")
  (if (empty? q)
      (println "<QUEUE-EMPTY>")
      (let ([next (first q)]
            [remain (rest q)])
        (set! q remain)
        (next 0))))

(define (test1)
  (if (spawn)
      (begin (println "In fiber 1")
             (terminate))
      (begin (println "In fiber 2")
             (terminate))))


(define (test2)
  (if (spawn)
      (begin (println "In fiber 1")
             (yield)
             (println "Back in fiber 1")
             (terminate))
      (begin (println "In fiber 2")
             (yield)
             (println "Back in fiber 2")
             (terminate))))


(define (test3)
  (if (spawn)
      (if (spawn)
          (begin (println "In fiber 1")
                 (terminate))
          (begin (println "In fiber 2")
                 (yield)
                 (println "Back in fiber 2")))
      (if (spawn)
          (begin (println "In fiber 3")
                 (yield))
          (begin (println "In fiber 4")
                 (println "Still in fiber 4"))))
  (println "Cleaning up")
  (terminate))