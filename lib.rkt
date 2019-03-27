#lang racket

(provide merge tree-fold check-tree
         flip tee map-h/v copy-annotations
         context extend lookup wrap ann-lookup
         compose-ctxtrs base-ctxtr
         strip-ctxtr make-strip-src-ctxtr make-negated-strip-src-ctxtr destroy-ctxtr length-ctxtr
         compose-checkers base-checker note-checker measure-checker
         children tree-type)

(define ☠ error)
(define (tee expr) (println expr) expr)

;; Tree =
;;   | H [List Tree ...]
;;   | V [List Tree ...]
;;   | Hole
;;   | Val value
;;   | A Symbol Any ... Tree

(define-syntax flip
  (syntax-rules ()
    [(_ fun) (λ (y) (λ (x) (fun x y)))]
    [(_ fun arg) ((flip fun) arg)]))
;; Tree type
(define tree-type car)
;; H & V
(define children cdr)
;; Value
(define value cadr)
;; Annotation
(define ann-key caadr)
(define info cdadr)
(define anned caddr)

(define (merge professor student)
 
  (define (merge-h-structure prof-h student-h plengths slengths)
    (define (merge-h-structure prof-h student-h plengths slengths new-h new-lengths)
      (define sum (foldr + 0 new-lengths))
      (cond
        [(null? plengths) (reverse (rest new-h))]
        [(> sum (car plengths)) (☠ 'merge "professor and student h lengths could not be merged")]
        [(= sum (car plengths))
         (define new-student `(h ,@(reverse (car new-h))))
         (define real-lengths (map (flip / (car plengths)) (reverse new-lengths)))
         (merge-h-structure
          (cdr prof-h)
          student-h
          (cdr plengths)
          slengths
          (cons
           '()
           (cons
            (merge
             `(ann (lengths ,real-lengths is) ,(car prof-h))
             `(ann (lengths ,real-lengths is) ,new-student)
             #f #f)
            (cdr new-h)))
          '())]
        [else
         (merge-h-structure
          prof-h
          (cdr student-h)
          plengths
          (cdr slengths)
          (cons (cons (car student-h) (car new-h)) (cdr new-h))
          (cons (car slengths) new-lengths))]))
    (merge-h-structure prof-h student-h plengths slengths '(()) '()))

  (define (get-lengths li len who)
    (cond
      [li
       (unless (= (length li) len)
         (☠ 'merge (format "annotated lengths for ~s do not correspond to actual values" who)))
       li]
      [else (make-list len (/ 1 len))]))
          
  (define (merge professor student (plengths #f) (slengths #f))
    (match* (professor student)
      [(`(ann (,args ...) ,annotated) tree)
       (if (eq? (car args) 'lengths)
           (merge annotated tree (cadr args) slengths)
           `(ann ,(append args (list 'professor))
                 ,(merge annotated tree plengths slengths)))]
      [(tree `(ann (,args ...) ,annotated))
       (if (eq? (car args) 'lengths)
           (merge tree annotated plengths (cadr args))
           `(ann ,(append args (list 'student))
                 ,(merge tree annotated plengths slengths)))]
      [(`(h ,children1 ...) `(h ,children2 ...))
       (match-define `(,plen ,slen) (map length `(,children1 ,children2)))
       (match-define `(,real-plengths ,real-slengths)
         (map get-lengths `(,plengths ,slengths) `(,plen ,slen) '(professor student)))
       `(ann
         (lengths ,real-plengths is analyzer)
         ,(cond
            [(equal? real-plengths real-slengths) `(h ,@(map merge children1 children2))]
            [(< plen slen) `(h ,@(merge-h-structure children1 children2 real-plengths real-slengths))]
            [else (☠ 'merge "professor and student h lengths do not correspond")]))]
      [(`(v ,children1 ...) `(v ,children2 ...))
       (if (not (or plengths slengths))
           `(v ,@(map merge children1 children2))
           (☠ 'merge "lengths applied to non-h"))]
      [('(hole) `(h ,children ...))
       (define real-lengths (get-lengths slengths (length children) 'student))
       `(ann
         (lengths ,real-lengths is analyzer)
         (h ,@(map (curry merge '(hole)) children)))]
      [('(hole) `(v ,children ...))
       (if (not slengths)
           `(v ,@(map (curry merge '(hole)) children))
           (☠ 'merge "lengths applied to non-h"))]
      [('(hole) `(value ,val)) `(value ,val)]))
  (merge professor student #f #f))

(define (copy-annotations t1 t2)
  (match t1
    [`(ann ,ann ,anned) `(ann ,ann ,(copy-annotations anned t2))]
    [else t2]))

(define (compose-results r f) (match (f (car r)) [`(,r2 ,e) (list r2 (append (cadr r) e))]))

(define (combine-results combiner . rs)
  (define vals (map car rs))
  (define errs (map cadr rs))
  (list (apply combiner vals) (apply append errs)))

(define (base-result v) `(,v ()))

(define (compose-ctxtrs c1 c2) (λ (tree acc) (c1 tree acc c2)))

(define (base-ctxtr tree acc) `(,tree ,acc))

;; I don't know what this is but I like it
(define (strip-ctxtr tree acc k)
  (match tree
    [`(ann (,key ,rest ...) ,anned)
     (strip-ctxtr anned (cadr (k tree acc)) k)]
    [else `(,tree ,acc)]))

(define ((make-src-stripper p) src)
  (define (strip-src-ctxtr tree acc k)
    (match tree
      [`(ann (,key ,val ... ,the-src) ,anned)
       (cond
         [(p the-src src) (strip-src-ctxtr anned (cadr (k tree acc)) k)]
         [else
          (match-define `(,ntree ,ctxt) (strip-src-ctxtr anned (cadr (k tree acc)) k))
          `((ann (,key ,@val ,the-src) ,ntree) ,ctxt)])]
      [else `(,tree ,acc)]))
  strip-src-ctxtr)

(define make-strip-src-ctxtr (make-src-stripper eq?))

(define make-negated-strip-src-ctxtr (make-src-stripper (compose not eq?)))

(define destroy-ctxtr (compose-ctxtrs strip-ctxtr base-ctxtr))

(define (context . pairs) pairs)

(define (extend ctxt . pairs)
  (append pairs ctxt))

(define (lookup key ctxt)
  (define result (assq key ctxt))
  (and result (second result)))

(define (wrap tree . anns) (foldr (λ (ann acc) `(ann ,ann ,acc)) tree anns))

(define (ann-lookup key tree)
  (and (eq? (tree-type tree) 'ann)
       (if (eq? (ann-key tree) key)
           (info tree)
           (ann-lookup key (anned tree)))))

(define (length-ctxtr tree acc k)
  (match-define `(,ntree ,nacc)
    (match tree
      [`(ann (lengths (,lens ...) is analyzer) ,anned)
       `(,tree ,(extend acc `(lengths ,lens)))]
      [`(h ,children ...)
       `((h
          ,@(map
             (λ (c l) `(ann (rel-length ,l is analyzer) ,c))
             children
             (lookup 'lengths acc)))
         ,acc)]
      [`(ann (rel-length ,len is analyzer) ,anned)
       `(,tree ,(extend acc `(rel-length ,len)))]
      [`(ann (length ,len is professor) ,anned)
       ;; this is inconsistent
       `(,tree ,(extend acc `(length ,len)))]
      [else `(,tree ,acc)]))
  (define len (lookup 'length acc))
  (define rel-len (lookup 'rel-length acc))
  (k ntree
     (if (and len rel-len)
         (extend nacc `(length ,(* len rel-len)))
         nacc)))

(define (compose-checkers c1 c2) (λ (ann ctxt return) (c1 ann ctxt return c2)))

(define (base-checker ann ctxt return) base-result)

(define (note-checker ann ctxt return continue)
  (match ann
    [`(note ,ann-pitch ,ann-acc ,ann-oct is professor)
     (λ (tree)
       (match (car (destroy-ctxtr tree '()))
         [`(value (note ,pitch ,acc ,oct))
          (if (and (eq? pitch ann-pitch) (eq? acc ann-acc) (eq? oct ann-oct))
              `((ann (note ,pitch ,acc ,oct is analyzer) tree) ())
              `(,tree ("incorrect note")))]
         [else (☠ 'note-checker (format "illegal application of note annotation to non-value ~s" tree))]))]
    [else (continue ann ctxt return)]))

(define (measure-checker ann ctxt return continue)
  (match ann
    [`(measure is professor)
     (λ (tree)
       (define len (lookup 'length ctxt))
       (if (eqv? len 1)
           `((ann (measure is analyzer) ,tree) ())
           `(,tree ("not a measure"))))]
    [else (continue ann ctxt return)]))

(define (default-combiner _ c1 c2)
  (map-h/v (curry cons c1) c2))

;; fold over a tree, defaults are identity.
(define (tree-fold
         tree
         #:her (her default-combiner)
         #:ver (ver default-combiner)
         #:after-h (after-h (λ (_ h) h))
         #:after-v (after-v (λ (_ v) v))
         #:anner (anner (λ (_ ann anned) `(ann ,ann ,anned)))
         #:valuer (valuer (λ (_ val) `(value ,val)))
         #:holer (holer (λ (_) '(hole)))
         #:base (base (λ (typ) `(,typ)))
         #:ctxtr (ctxtr base-ctxtr)
         #:ctxt (ctxt '()))
  (define (tree-fold ctxt tree)
    (match-define `(,ntree ,nctxt) (ctxtr tree ctxt))
    (match ntree
      [`(h ,children ...)
       (after-h
        nctxt
        (foldr (curry her nctxt) (base 'h) (map (curry tree-fold nctxt) children)))]
      [`(v ,children ...)
       (after-v
        nctxt
        (foldr (curry ver nctxt) (base 'v) (map (curry tree-fold nctxt) children)))]
      [`(ann ,ann ,anned) (anner nctxt ann (tree-fold nctxt anned))]
      [`(value ,val) (valuer nctxt val)]
      ['(hole) (holer nctxt)]))
  (tree-fold ctxt tree))

;; apply a funtion to the list of children in an `h` or a `v`
(define (map-h/v fun tree)
  (match tree
    [`(,(and typ (or 'h 'v)) ,children ...)
     `(,typ ,@(fun children))]))

(define (check-tree tree checker ctxtr)
  (define (combiner _ c t)
    (combine-results (λ (c t) (map-h/v (curry cons c) t)) c t))
  (define (anner ctxt ann checked)
    (combine-results
     (λ (anned) `(ann ,ann ,anned))
     (compose-results checked (let/ec k (checker ann ctxt k)))))
  (define (valuer _ val) `((value ,val) ()))
  (tree-fold
   tree
   #:her combiner
   #:ver combiner
   #:anner anner
   #:valuer valuer
   #:ctxtr ctxtr
   #:base (λ (typ) (base-result `(,typ)))))

(define example
  `(ann (length 2 is professor)
        (ann (lengths (1/2 1/2) is analyzer)
             (h (ann (measure is professor) (ann (note b 0 4 is professor) (value (note a 0 4))))
                (ann (measure is professor) (ann (note b 0 4 is professor) (value (note b 0 4))))))))

(define checker (foldl compose-checkers base-checker `(,measure-checker ,note-checker)))
(define ctxtr (compose-ctxtrs length-ctxtr base-ctxtr))
#;(check-tree example checker ctxtr)