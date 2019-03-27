#lang racket/unit

(require drracket/tool
         racket/class
         racket/gui/base)

(import drracket:tool^)
(export drracket:tool-exports^)

(define test-mixin
  (mixin (drracket:unit:frame<%>) ()
    (super-new)
    (inherit get-insert-menu
             get-editor
             get-button-panel
             register-toolbar-button)
    (new menu-item%
         [parent (get-insert-menu)]
         [label "Insert Tree"]
         [callback
          (λ (i e)
            (define editor (get-editor))
            (send editor insert
                  (new (dynamic-require 'tonart/editor 'test-snip%))))])))
(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-unit-frame test-mixin)
(send (get-the-snip-class-list) add (dynamic-require 'tonart/editor 'test-snip-class))
