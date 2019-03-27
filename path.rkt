#lang racket
(require "lib.rkt")
(provide path->tree parent-path child-path cycle-path
         add-child-at-path remove-at-path nullify-at-path
         h-split-at-path v-split-at-path
         annotations-at-path set-annotations-at-path)

(define (path->tree path tree)
  (for/fold ([tree tree])
            ([i path])
    (match-define `(,ntree ,_) (destroy-ctxtr tree '()))
    (match ntree
      [`(,(or 'h 'v) ,children ...) (list-ref children i)]
      [else (error 'path->tree "invalid path")])))

(define (parent-path path)
  (if (null? path) path (take path (sub1 (length path)))))

(define (child-path path tree)
  (define subtree (car (destroy-ctxtr (path->tree path tree) '())))
  (match subtree
    [`(,(or 'h 'v) ,children ...) (append path (list 0))]
    [else path]))

(define (cycle-path val path tree)
  (cond
    [(null? path) path]
    [else
     (define child (last path))
     (define parent (parent-path path))
     (define subtree (car (destroy-ctxtr (path->tree parent tree) '())))
     (define len (length (children subtree)))
     (if (zero? len)
         (parent-path path)
         (append parent (list (modulo (+ child val) (length (children subtree))))))]))

(define (list-remove li i)
  (define-values (head tail) (split-at li i))
  (append head (cdr tail)))

(define (apply-at-path fn path tree)
  (define (helper path tree)
    (match tree
      [`(ann ,ann ,anned) `(ann ,ann ,(helper path anned))]
      [else 
       (cond
         [(null? (cdr path)) (fn (car path) tree)]
         [else
          (match tree
            [`(,(and typ (or 'h 'v)) ,children ...)
             `(,typ ,@(list-update children (car path) (curry helper (cdr path))))]
            [else (error 'path->tree "invalid path")])])]))
  (helper path tree))

(define (remove-at-path path tree)
  (if (null? path)
      (copy-annotations tree '(hole))
      (apply-at-path
       (λ (idx tree)
         (match tree
           [`(,(or 'v 'h) ,children ...)
            (if (null? (cdr children))
                '(hole)
                (map-h/v (λ (ch) (list-remove ch idx)) tree))]))
       path
       tree)))
  
(define (nullify-at-path path tree)
  (if (null? path)
      (copy-annotations tree '(hole))
      (apply-at-path
       (λ (idx tree)
         (map-h/v
          (λ (ch)
            (define base (copy-annotations (list-ref ch idx) '(hole)))
            (list-set ch idx base))
          tree))
       path
       tree)))

;; these two require some abstraction
(define (h-split-at-path path tree)
  (define (split-matcher tree)
    (match tree
      [`(h ,children ...) `(h ,tree ,tree)]
      [`(v ,children ...) `(h ,tree ,tree)]
      [`(ann ,ann ,anned) `(ann ,ann ,(split-matcher anned))]
      [`(value ,val) `(h (value ,val) (hole))]
      ['(hole) '(h (hole) (hole))]))
  (if (null? path)
      (split-matcher tree)
      (apply-at-path
       (λ (idx tree)
         (map-h/v
          (λ (ch) (list-set ch idx (split-matcher (list-ref ch idx))))
          tree))
       path
       tree)))

(define (v-split-at-path path tree)
  (define (split-matcher tree)
    (match tree
      [`(h ,children ...) `(v ,tree ,tree)]
      [`(v ,children ...) `(v ,tree ,tree)]
      [`(ann ,ann ,anned) `(ann ,ann ,(split-matcher anned))]
      [`(value ,val) `(v (value ,val) (hole))]
      ['(hole) '(v (hole) (hole))]))
  (if (null? path)
      (split-matcher tree)
      (apply-at-path
       (λ (idx tree)
         (map-h/v
          (λ (ch) (list-set ch idx (split-matcher (list-ref ch idx))))
          tree))
       path
       tree)))

(define (add-child-at-path path tree)
  (define (add-child subtree)
    (match subtree
      [`(,(or 'h 'v) ,children ...) (map-h/v (λ (ch) (append ch (list '(hole)))) subtree)]
      [`(ann ,ann ,anned) `(ann ,ann ,(add-child anned))]
      [_ subtree]))
  (if (null? path)
      (add-child tree)
      (apply-at-path
       (λ (idx tree)
         (map-h/v
          (λ (ch)
            (define subtree (list-ref ch idx))
            (list-set
             ch
             idx
             (add-child subtree)))
          tree))
       path
       tree)))

(define (annotations-at-path path tree #:filter [fillter base-ctxtr])
  (define (get-annotations tree)
    (match tree
      [`(ann ,ann ,anned) (cons ann (get-annotations anned))]
      [else '()]))
  (get-annotations (first (fillter (path->tree path tree) '()))))

(define (set-annotations-at-path anns path tree #:filter [fillter destroy-ctxtr])
  (define (wrap-in-annotations tree)
    (foldr (λ (ann acc) `(ann (,(car ann) ,@(cdr ann)) ,acc)) (car (fillter tree '())) anns))
  (if (null? path)
      (wrap-in-annotations tree)
      (apply-at-path
       (λ (idx tree)
         (map-h/v
          (λ (ch)
            (define subtree (list-ref ch idx))
            (list-set ch idx (wrap-in-annotations subtree)))
          tree))
         path
         tree)))