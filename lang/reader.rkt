#lang s-exp syntax/module-reader
tonart

#:read read
#:read-syntax read-syntax

(define (read-syntax path port)
  #'42)
