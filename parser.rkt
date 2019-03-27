#lang racket
(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide string->score)

(define-tokens music
  (id colon note rest measure sharp flat octave lbrocket rbrocket number slash eof))

(define get-token
  (lexer-src-pos
   ["+" (token-sharp lexeme)]
   ["-" (token-flat lexeme)]
   [">" (token-rbrocket lexeme)]
   ["<" (token-lbrocket lexeme)]
   ["o" (token-octave lexeme)]
   [":" (token-colon lexeme)]
   [(:+ numeric) (token-number (string->number lexeme))]
   ["/" (token-slash lexeme)]
   [(:or "a" "b" "c" "d" "e" "f" "g") (token-note (string->symbol lexeme))]
   ["r" (token-rest lexeme)]
   ["|" (token-measure lexeme)]
   [(:+ alphabetic) (token-id (string->symbol lexeme))]
   [(eof) (token-eof lexeme)]
   [whitespace (return-without-pos (get-token input-port))]))

(define (tokenizer src) (λ () (get-token src)))

#|
(define my-str
  (open-input-string #<<blep
flute:    o5 c  c8 < b- a-4 > c <  | b-    b-8 a- g4   b-
clarinet: o4 e- e-8  d8 c d   e- c | d2           c4   e-8 d
bassoon:  o2 c  r  <    f     a-   | g     r      e- r

flute:    r8 a b > c  d4    d8 c  < | b1
clarinet: c2          f4    e-      | d1
bassoon:  f    g      a-    b-      | g1

blep
                     ))|#

(define the-str #<<blep
flute:     o5 e-  d     c < b   | b-     a        a-     g > | c1
oboe:         g   f     e-  g   | g      f+       f      g   | g1
clarinet:     c < b >   c   d   | c      c <      b      d   | e-1
bassoon:   o2 c   d     e-  f   | e      e-       d      b   | c1
blep
  )

#|
(h
  1/3 (h : measure = []
       1/4 (v '(e- g c c))
       1/4 (v '(d f d b))
       1/4 (v '(c e- c e-))
       1/4 (v '(b g d f)))
  1/3 (h : measure = []
       1/4 (v '(b- g c e))
       1/4 (v '(a f+ c e-))
       1/4 (v '(a- f b d))
       1/4 (v '(b d g g)))
  1/3 (v : measure = [] '(e- g c c)))
|#

(define my-str (open-input-string the-str))

(port-count-lines! my-str)

(define my-parser
  (parser
   (grammar
    (label [(id colon) $1])
    (voices
     [(label music voices) (cons (list 'voice $1 $2) $3)]
     [(label music) (list (list 'voice $1 $2))])
    (rational
     [(number slash number) (/ $1 $3)]
     [(number) $1])
    (note-root
     [(note) $1]
     [(note-root flat) (list 'flat $1)]
     [(note-root sharp) (list 'sharp $1)])
    (music
     [(note-root rational) (list (list 'note $1 (/ 1 $2)))]
     [(note-root rational music) (cons (list 'note $1 (/ 1 $2)) $3)]
     [(note-root) (list (list 'note $1 #f))]
     [(note-root music) (cons (list 'note $1 #f) $2)]
     [(rest rational) (list (list 'rest (/ 1 $2)))]
     [(rest rational music) (cons (list 'rest (/ 1 $2)) $3)]
     [(rest) (list (list 'rest #f))]
     [(rest music) (cons (list 'rest #f) $2)]
     [(octave number) (list (list 'octave $2))]
     [(octave number music) (cons (list 'octave  $2) $3)]
     [(measure) (list 'measure)]
     [(measure music) (cons 'measure $2)]
     [(lbrocket) (list 'shift-down)]
     [(lbrocket music) (cons 'shift-down $2)]
     [(rbrocket) (list 'shift-up)]
     [(rbrocket music) (cons 'shift-up $2)]))
   (tokens music)
   (src-pos)
   (start voices)
   (end eof)
   (error (λ (tok-ok? tok-name tok-value start-pos end-pos)
            (error 'parse
                   (format "~s ~s ~s ~s ~s"
                           tok-ok? tok-name tok-value start-pos end-pos))))))

(define (concat-voices voices)
  (for/fold ([acc '()])
            ([voice voices])
    (match voice
      [`(voice ,name ,music)
       (if (dict-has-key? acc name)
           (dict-update acc name (λ (m) (append m music)))
           (dict-set acc name music))])))

;; give every note a duration, filling them in from provided durations
;; (default is 1/4)
;; Give every note an octave from the marked octaves/shifts, deleting these
;; tokens (default is 4)
;; Give each note a voice, removing the initial voice marker, turning
;; the structure into a list of melodies
;; convert accidentals [(flat (sharp (sharp pitch)))] into a number
;; [1 in this case], put it in with the note
(define (create-values music)
  
  (define (eval-accidental pitch)
    (match pitch
      [`(,(and mod (or 'flat 'sharp)) ,rest)
       (match (eval-accidental rest)
         [`(,note ,acc) `(,note ,((if (eq? mod 'sharp) add1 sub1) acc))])]
      [pitch `(,pitch 0)]))
  
  (for/list ([voice (in-list music)])
    (define voice-name (car voice))
    (define-values (o d notes)
      (for/fold ([octave 4] [duration 1/4] [notes '()])
                ([note (in-list (cdr voice))])
      (match note
        [`(note ,value ,maybe-dur)
         (define dur (or maybe-dur duration))
         (define valued
           `(ann (length ,dur is)
                 (ann (voice ,voice-name is)
                      (value (note ,@(eval-accidental value) ,octave)))))
         (values octave dur (cons valued notes))]
        [`(octave ,oct) (values oct duration notes)]
        [(and mod (or 'shift-up 'shift-down))
         (values ((if (eq? mod 'shift-up) add1 sub1) octave) duration notes)]
        ['measure (values octave duration (cons 'measure notes))])))
    (reverse notes)))

(define parse-result (my-parser (tokenizer my-str)))
(define voices-concated (concat-voices parse-result))
(define values-created (create-values voices-concated))

;; Wrap each measure in a list and get rid of measure tokens
(define (group-measures music)
  (define (☺ music)
    (match music
      [(list 'measure rest ...)
       (cons '() (☺ rest))]
      [(list other rest ...)
       (define rest-☺ (☺ rest))
       (cons (cons other (car rest-☺)) (cdr rest-☺))]
      [_ '(())]))
  (map ☺ music))

(define measures-grouped (group-measures values-created))

;; Group each set of corresponding notes from given voices vertically.
;; I would like to say that all notes in the vertical have to be the same
;; length but the fact of the matter is I think the lists just have to be
;; the same length... This will be revised heavily.
(define (group-vertical music)
  (define (handle-voices . v)
    (if (null? (cdr v))
        `(h ,@(car v))
        `(h ,@(apply map (λ m `(v ,@m)) v))))
  (apply map handle-voices music))

(define verticals-grouped (group-vertical measures-grouped))

;; annotate each tree in the given list as a measure
(define (combine-measures music)
  (if (null? (cdr music)) (car music) `(h ,@(map (λ (t) `(ann (measure is) ,t)) music))))

(define final-tree (combine-measures verticals-grouped))

;; add length annotations to the h's, assuming the children of each h get an equal proportion
;; of the total length ( :( ).
(define (create-lengths music)
  (match music
    [`(h ,children ...)
     (define len (length children))
     (define lengths (make-list len (/ 1 len)))
     `(ann (lengths ,lengths) (h ,@(map create-lengths children)))]
    [`(v ,children ...)
     `(v ,@(map create-lengths children))]
    [else music]))

(define done (create-lengths final-tree))

(define (count-lines-port p) (port-count-lines! p) p)

(define string->score
  (compose
   create-lengths
   combine-measures
   group-vertical
   group-measures
   create-values
   concat-voices
   my-parser
   tokenizer
   count-lines-port
   open-input-string))

(define parse-only
  (compose
   my-parser
   tokenizer
   count-lines-port
   open-input-string))

;; This poses some problems:
;; 1. The violin melody is not uniform with the accompaniment.
;; 2. The harmonic rhythm alternates between half and quarter (except for the end)
;; 3. The piece is in 3/4 (is that a problem?)
;;
;; group verticals ought to be able to group by the longest of the notes,
;; producing a list of lists which can later be turned into h's and values
;;
;;   note: there's still a glaring issue exemplified by suspensions... we can't
;;   cut suspended notes in half, otherwise they look exactly like two notes.
;;
;;   The solutions I have thought of are either to represent a tie using an
;;   annotation, represent a tie using a special "tied-value" struct, or use the
;;   sum of all contiguous notes to group verticals.
;;
;; some silly things could be made to happen by syncopating the bass so the other
;; voices end up determining where the verticals are drawn, I think.
;;
(define the-str2 #<<blep
violin:  o5 d8/3 < b8 > c+  d | e8/3  c+8 d   c+ < | b8/3 a8 g4  | f+4/3
soprano:    b2          b4    | a+2       b4       | b2      a4  | a4/3
alto:       f+2         g4    | f+2       f+4      | g2      g4  | f+4/3
tenor:      d2          c+4   | c+2       d4       | d2      c+4 | d4/3
bass:    o3 b2          b4    | b2        b4       | g2      a4  | d4/3
blep
  ;;        i           iio/65  V         i          VI      
  )
#|
(h
  1/4 (h : measure = []
       2/3 (v ((h 3/8 d 1/8)    b f+ d b))
       1/3 (v ((h 1/8 c+ 1/8 d) b g c+ e)))
  1/4 (h : measure = []
       2/3 (v ((h 3/8 e 1/8 c+) a+ f+ c+ f+))
       1/3 (v ((h 1/8 d 1/8 c+) b  f+ d  b)))
  1/4 (h : measure = []
       2/3 (v ((h 3/8 b 1/8 a) b g d b))
       1/3 (v (g               a  g  c+ a)))
  1/4 (v : measure = [] (f+ a f+ d d)))
|#

;;(string->score the-str2)
;;(parse-only the-str2)