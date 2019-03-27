#lang racket

(require racket/promise
         racket/snip
         racket/runtime-path
         wxme
         "ui.rkt"
         (only-in racket [read r:read])
         (only-in racket/draw read-bitmap))
   
(provide test-snip% test-snip-class reader)

(define-runtime-path bmp-path "button.png")
(define snip-bmp (read-bitmap bmp-path))
(define WIDTH (+ (send snip-bmp get-width) 2))
(define HEIGHT (+ (send snip-bmp get-height) 2))

(define test-snip%
  (class* snip% (readable-snip<%>)
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init-field [tree '(hole)])
      
    (super-new)
    (set-snipclass test-snip-class)
    (send (get-the-snip-class-list) add test-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
      
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (when w (set-box! w WIDTH))
      (when h (set-box! h HEIGHT))
      (when descent (set-box! descent 1))
      (when space (set-box! space 1))
      (when lspace (set-box! lspace 1))
      (when rspace (set-box! rspace 1)))
      
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-bitmap snip-bmp x y))
      
    (define/override (copy)
      (new test-snip% [tree tree]))
      
    (define/override (write f)
      (define to-write (~s tree))
      (send f put (string-length to-write) (string->bytes/utf-8 to-write)))
      
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (set! tree (edit-tree tree))))

    (define/public (read-special source line column position)
      (datum->syntax #f `',tree (list source line column position #f)))))
   
(define test-snip-class%
  (class snip-class%
    (inherit set-classname)
      
    (super-new)
    (set-classname (~s '((lib "editor.rkt" "tonart")
                         (lib "editor.rkt" "tonart"))))
      
    (define/override (read f)
      (define tree (with-input-from-string (bytes->string/utf-8 (send f get-unterminated-bytes)) r:read))
      (new test-snip% 
           [tree tree]))))
   
(define test-snip-class (new test-snip-class%))

(define test-reader%
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (define tree (send stream read-raw-bytes "test-snip"))
      (cond
        [text-only?
         (string->bytes/utf-8 (~s tree))]
        [else
         (new test-readable [tree tree])]))
    (super-new)))
   
(define test-readable
  (class* object% (readable<%>)
    (init-field tree)
    (define/public (read-special source line column position)
      (datum->syntax #f `',tree (list source line column position #f)))
    (super-new)))
   
(define reader (new test-reader%))
