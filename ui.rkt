#lang racket/gui
(require "lib.rkt")
(require "path.rkt")

(provide edit-tree)

(define (size-ctxtr tree acc k)
  (define (apply-dim children name transform)
    (define len (length children))
    (define dim (lookup name acc))
    (map (λ (c) `(ann (,name ,(transform dim) is renderer) ,c)) children))
  (match tree
    [`(ann (,(and name (or 'width 'height)) ,dim is renderer) ,anned)
     `(,tree ,(extend acc `(,name ,dim)))]
    [`(h ,children ...)
     (k
      `(h
        ,@(apply-dim
           (apply-dim children 'width (flip / (length children)))
           'height
           (curry * 2/3)))
      acc)]
    [`(v ,children ...)
     (k
      `(v
        ,@(apply-dim
           (apply-dim children 'width (flip - 20))
           'height
           (compose (flip - 20) (flip / (length children)))))
      acc)]
    [else (k tree acc)]))

(define (position-ctxtr tree acc k)
  (match tree
    [`(ann (,(and ax (or 'x 'y)) ,pos is renderer) ,anned)
     `(,tree ,(extend acc `(,ax ,pos)))]
    [`(h ,children ...)
     (define width (lookup 'width acc))
     (define height (lookup 'height acc))
     (define cwidth (/ width (length children)))
     (define myx (lookup 'x acc))
     (define myy (lookup 'y acc))
     (define cy (+ myy (* height 1/3)))
     (define children*
       (for/list ([c children]
                  [i (in-naturals)])
         (define width-ann `(x ,(+ myx (* i cwidth)) is renderer))
         (define height-ann `(y ,cy is renderer))
         (wrap c width-ann height-ann)))
     `((h ,@children*) ,acc)]
    [`(v ,children ...)
     (define height (lookup 'height acc))
     (define myx (lookup 'x acc))
     (define myy (lookup 'y acc))
     (define cheight (/ height (length children)))
     (define children*
       (for/list ([c children]
                  [i (in-naturals)])
         (define width-ann `(x ,(+ myx 10) is renderer))
         (define height-ann `(y ,(+ myy (* i cheight) 10) is renderer))
         (wrap c width-ann height-ann)))
     `((v ,@children*) ,acc)]
    [else (k tree acc)]))

(define (semitones->accidental tones)
  (match tones
    [0 ""]
    [1 "♯"]
    [-1 "♭"]))

(define (get-dims lookup ctxt) (map (flip lookup ctxt) '(x y width height)))

;; combine these contextors to 1. get the first dimensions from a tree and 2. carry
;; the dimensions as they are found

(define (basic-ctxtr tree acc)
  (match tree
    [`(ann (,(and key (or 'width 'height 'x 'y)) ,val is renderer) ,anned)
     `(,tree ,(extend acc `(,key ,val)))]
    [else `(,tree ,acc)]))

(define (annotate-dims tree)
  (tree-fold
   tree
   #:ctxtr (foldl compose-ctxtrs base-ctxtr (list position-ctxtr size-ctxtr))))

(define strip-render-ctxtr (make-strip-src-ctxtr 'renderer))
(define initial-ctxtr (compose-ctxtrs strip-render-ctxtr basic-ctxtr))

(define (strip-render-anns tree)
  (tree-fold tree #:ctxtr (compose-ctxtrs strip-render-ctxtr base-ctxtr)))

(define (gen-bitmap tree)
  (match-define `(,itree ,ictxt) (initial-ctxtr tree '()))
  (match-define `(,width ,height) (map (flip lookup ictxt) '(width height)))
  (define bmp (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap bmp]))
  (send dc set-pen
        (send the-pen-list find-or-create-pen
              "black" 5 'solid 'round))
  (send dc set-smoothing 'smoothed)
  (define (her ctxt c1 c2)
    (match-define (list x y w h) (get-dims lookup ctxt))
    (match-define (list cx cy cw ch) (get-dims (compose car ann-lookup) c1))
    (send dc draw-line (+ x (/ w 2)) y (+ cx (/ cw 2)) cy)
    (map-h/v (curry cons c1) c2))
  ;; v does not depend on children, so using after-v instead of ver
  (define (after-v ctxt v)
    (match-define (list x y w h) (get-dims lookup ctxt))
    (send dc draw-line x y x (+ y h))
    (send dc draw-line (+ x w) y (+ x w) (+ y h))
    v)
  (define (anner ctxt ann anned) `(ann ,ann ,anned))
  (define (valuer ctxt value)
    (match-define (list x y w h) (get-dims lookup ctxt))
    (match value
      [`(note ,pitch ,acc ,octave)
       (define str (format "~a~a~a" pitch (semitones->accidental acc) octave))
       (define-values (tw th tb tv) (send dc get-text-extent str))
       (send dc draw-text str (+ x (/ w 2) (/ tw -2)) y)])
    `(value ,value))
  (define (holer ctxt)
    (match-define (list x y w h) (get-dims lookup ctxt))
    (send dc draw-rectangle (+ x (/ w 2) -10) y 20 20)
    '(hole)) 
  (tree-fold
   itree
   #:her her
   #:after-v after-v
   #:anner anner
   #:valuer valuer
   #:holer holer
   #:ctxtr basic-ctxtr
   #:ctxt ictxt)
  bmp)

(define in-rect
  (match-lambda**
   [(`(,x ,y) `(,rx ,ry ,rw ,rh))
    (not (or (< x rx) (> x (+ rx rw))
             (< y ry) (> y (+ ry rh))))]))

(define (position->path tree position)
  (define (helper tree ctxt acc)
    (match-define `(,ntree ,nctxt) (basic-ctxtr tree ctxt))
    (define dims (get-dims lookup nctxt))
    (and (in-rect position dims)
         (match ntree
           [`(,(or 'h 'v) ,children ...)
            (or
             (for/or ([c children]
                      [i (in-naturals)])
               (helper c nctxt (cons i acc)))
             (reverse acc))]
           [`(ann ,ann ,anned) (helper anned nctxt acc)]
           [(or `(value ,_) '(hole)) (reverse acc)])))
  (match-define `(,itree ,ictxt) (initial-ctxtr tree '()))
  (helper itree ictxt '()))

(define test
  `(h (value (note c 0 4))
      (v (value (note d 0 5))
         (h (value (note f 1 4)) (value (note e 0 4)))
         (v (value (note d 0 4))
            (value (note b 0 3))))
      (h (hole) (hole))
      (h (value (note f 1 4)) (value (note e 0 4)))))

(define (path->region path tree)
  (get-dims (compose car ann-lookup) (path->tree path tree)))

(define ann-dialog%
  (class dialog%
    (init-field tree-container ann-list)
    (define append #f)
    (define index #f)
    (super-new)
    (define key-field
      (new text-field%
           [parent this]
           [label "Key"]))
    
    (define value-field
      (new text-field%
           [parent this]
           [label "Value"]))

    (define mode-field
      (new choice%
           [parent this]
           [label "Mode"]
           [choices '("Positive" "Normative")]))

    (define (set-ann n)
      (define key (send key-field get-value))
      (define value (send value-field get-value))
      (define mode (send mode-field get-string-selection))
      (send ann-list set-string n key 0)
      (send ann-list set-string n value 1)
      (send ann-list set-string n mode 2)
      (send ann-list set-data n
            `(,(with-input-from-string key read)
              ,@(with-input-from-string (string-append "(" value ")") read)
              ,(match (with-input-from-string mode read)
                 ['Normative 'ought] ['Positive 'is]))))
    
    (define/augment (on-close)
      (cond
        [append
         (define len (get-field list-length ann-list))
         (send ann-list append "")
         (set-ann len)]
        [index (set-ann index)])
      (send tree-container refresh-anns)
      (send key-field set-value "")
      (send value-field set-value "")
      (send mode-field set-selection 0))
    
    (define/public (set-key! key)
      (send key-field set-value (~s key)))

    (define/public (set-values! values)
      (send value-field set-value (string-join (map ~s values))))
    
    (define/public (set-mode! mode)
      (send mode-field set-selection (match mode ['is 0] ['ought 1])))

    (define/public (open-for-append)
      (set! append #t)
      (set! index #f)
      (send this show #t))

    (define/public (open-for-set n)
      (set! append #f)
      (set! index n)
      (match-define `(,key ,values ... ,mode) (send ann-list get-data n))
      (send this set-key! key)
      (send this set-values! values)
      (send this set-mode! mode)
      (send this show #t))))

(define tree-canvas%
  (class canvas%
    (define/override (on-event event)
      (when (send event button-down? 'left)
        (define position
          `(,(+ (send event get-x) (get-field camera-x this))
            ,(+ (send event get-y) (get-field camera-y this))))
        (set-selected! (position->path (get-field tree this) position))
        (send this refresh)))
    (define/override (on-char event)
      (define key (send event get-key-code))
      (when (get-field selected this)
        (match key
          [#\h (set-field!
                tree
                this
                (init-tree
                 (strip-render-anns
                  (h-split-at-path
                   (get-field selected this)
                   (get-field tree this)))))
               (send this refresh)]
          [#\v (set-field!
                tree
                this
                (init-tree
                 (strip-render-anns
                  (v-split-at-path
                   (get-field selected this)
                   (get-field tree this)))))
               (send this refresh)]
          [#\+ (set-field!
                tree
                this
                (init-tree
                 (strip-render-anns
                  (add-child-at-path (get-field selected this) (get-field tree this)))))
               (send this refresh)]
          [#\r (set-field!
                tree
                this
                (init-tree
                 (strip-render-anns
                  (remove-at-path
                   (get-field selected this)
                   (get-field tree this)))))
               (set-selected! (cycle-path 0 (get-field selected this) (get-field tree this)))
               (send this refresh)]
          [#\n (set-field!
                tree
                this
                (nullify-at-path
                 (get-field selected this)
                 (get-field tree this)))
               (set-selected! (cycle-path 0 (get-field selected this) (get-field tree this)))
               (send this refresh)]
          [#\d (set-selected! (cycle-path 1 (get-field selected this) (get-field tree this)))
               (send this refresh)]
          [#\a (set-selected! (cycle-path -1 (get-field selected this) (get-field tree this)))
               (send this refresh)]
          [#\w (set-selected! (parent-path (get-field selected this)))
               (send this refresh)]
          [#\s (set-selected! (child-path selected tree))
               (send this refresh)]
          [#\@
           (send ann-dialog open-for-append)]
          ['escape (set-selected! #f)
                   (send this refresh)]
          [else (void)]))
      (when (not (get-field selected this))
        (match key
          [(or #\w #\a #\s #\d)
           (set-selected! '())
           (send this refresh)]
          [else (void)]))
      (match key
        [#\[ (set-field! t-width this (inexact->exact (ceiling (* (get-field t-width this) .9))))
             (set-field! t-height this (inexact->exact (ceiling (* (get-field t-height this) .9))))
             (set-field!
              tree
              this
              (init-tree
               (strip-render-anns
                (get-field tree this))))
             (send this refresh)]
        [#\] (set-field! t-width this (inexact->exact (floor (* (get-field t-width this) 1.1))))
             (set-field! t-height this (inexact->exact (floor (* (get-field t-height this) 1.1))))
             (set-field!
              tree
              this
              (init-tree
               (strip-render-anns
                (get-field tree this))))
             (send this refresh)]
        ['up (set-field! camera-y this (- (get-field camera-y this) 20))
             (send this refresh)]
        ['down (set-field! camera-y this (+ (get-field camera-y this) 20))
               (send this refresh)]
        ['left (set-field! camera-x this (- (get-field camera-x this) 20))
               (send this refresh)]
        ['right (set-field! camera-x this (+ (get-field camera-x this) 20))
                (send this refresh)]
        [else (void)]))
    (define/override (on-paint)
      (super on-paint)
      (define dc (send this get-dc))
      (send
       dc
       draw-bitmap-section
       (gen-bitmap (get-field tree this))
       0
       0
       (get-field camera-x this)
       (get-field camera-y this)
       (send this get-width)
       (send this get-height))
      (when (get-field selected this)
        (match-let ([(list x y w h)
                     (path->region
                      (get-field selected this)
                      (get-field tree this))])
          (send dc set-alpha .5)
          (send
           dc
           draw-rectangle
           (- x (get-field camera-x this))
           (- y (get-field camera-y this))
           w
           h)
          (send dc set-alpha 1))))
    (define (refresh-ann-list)
      (define anns
        (annotations-at-path
         (get-field selected this)
         (get-field tree this)
         #:filter (compose-ctxtrs strip-render-ctxtr base-ctxtr)))
      (define uniform-anns
        (map
         (match-lambda
           [`(,key ,vals ... ,mode)
            (list (~s key)
                  (string-join (map ~s vals))
                  (if (eq? mode 'is) "Positive" "Normative"))])
         anns))
      (define keys (map car uniform-anns))
      (define vals (map cadr uniform-anns))
      (define modes (map caddr uniform-anns))
      (send ann-list set keys vals modes)
      (for ([i (in-naturals)] [ann anns]) (send ann-list set-data i ann)))

    (define/public (refresh-anns)
      (define anns
        (for/list ([i (in-range (get-field list-length ann-list))])
          (send ann-list get-data i)))
      (set! tree (set-annotations-at-path anns selected tree
        #:filter (compose-ctxtrs (make-negated-strip-src-ctxtr 'renderer) base-ctxtr))))
    (define (set-selected! path)
      (set-field! selected this path)
      (cond
        [(not selected) (send ann-list set '() '() '())]
        [else (refresh-ann-list)]))
    (define (init-tree tree)
      (annotate-dims
       (wrap
        tree
        `(x 10 is renderer)
        `(y 10 is renderer)
        `(width ,(- (get-field t-width this) 20) is renderer)
        `(height ,(- (get-field t-height this) 20) is renderer))))
    (define/public (set-tree! t)
      (set-field! tree this (init-tree t)))
    (define/public (get-tree)
      (strip-render-anns (get-field tree this)))
    (super-new)

    (init-field tree)
    (init-field t-width)
    (init-field t-height)
    (init-field ann-list)
    (field [ann-dialog #f])
    (init-field [camera-x 0])
    (init-field [camera-y 0])
    (init-field [selected #f])
    (set! tree (init-tree (get-field tree this)))
    (define dc (send this get-dc))
    (send
     dc
     set-pen
     (send the-pen-list
           find-or-create-pen
           "black" 5 'solid 'round))
    (send dc set-brush "black" 'solid)))

(define ann-list%
  (class list-box%
    (field [ann-dialog #f]
           [list-length 0])
    
    (super-new)

    (define/override (append value (data #f))
      (super append value data)
      (set! list-length (add1 list-length)))

    (define/override (set . choices)
      (super set . choices)
      (set! list-length (length (car choices))))
    
    (define/override (on-subwindow-char _ event)
      (match (send event get-key-code)
        [#\e
         (define selected-index? (send this get-selections))
         (when selected-index?
           (send ann-dialog open-for-set (car selected-index?)))]
        [else (void)]))))

(define writer-frame%
  (class dialog%
    (init-field [output-thunk (λ () #f)])
    (super-new)))

(define FRAME-WIDTH 1200)
(define FRAME-HEIGHT 800)

(define VIRTUAL-WIDTH 800)
(define VIRTUAL-HEIGHT 600)

(define (make-tree-editor tree)
  (define frame
    (parameterize ([current-eventspace (make-eventspace)])
      (new writer-frame%
           [label "Example"]
           [min-width FRAME-WIDTH]
           [min-height FRAME-HEIGHT])))

  (define panel
    (new horizontal-panel%
         [parent frame]
         [alignment '(center center)]))

  (define ann-list
    (new ann-list%
         [parent panel]
         [choices '()]
         [label "Annotations"]
         [style '(multiple vertical-label column-headers)]
         [columns '("Key" "Value" "Mode")]))
  
  (define tree-canvas
    (new tree-canvas%
         [parent panel]
         [tree tree]
         [t-width VIRTUAL-WIDTH]
         [t-height VIRTUAL-HEIGHT]
         [min-width 800]
         [min-height 600]
         [ann-list ann-list]))

  (define ann-dialog
    (new ann-dialog%
       [label "Edit Annotation"]
       [tree-container tree-canvas]
       [ann-list ann-list]))

  (set-field! ann-dialog tree-canvas ann-dialog)
  (set-field! ann-dialog ann-list ann-dialog)
  
  (set-field! output-thunk frame (λ () (send tree-canvas get-tree)))
  
  frame)

(define (edit-tree t)
  (define frame (make-tree-editor t))
  (send frame show #t)
  ((get-field output-thunk frame)))
