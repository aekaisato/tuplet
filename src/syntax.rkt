#lang racket

(require (prefix-in rt: "runtime.rkt") (prefix-in rsound: rsound) syntax-spec-v3 (for-syntax syntax/parse racket/syntax))

(provide track tlet)

;                                                              ;;                                         
;                            ;;                                ;;                                         
;                            ;;                                ;;                                         
;    ;;;;  ;;;  ;;   ;;;;   ;;;;;  ;;;;;  ;;; ;;;       ;;;;   ;;  ;;;;;    ;;;;    ;;;;    ;;;;    ;;;;  
;   ;;;;;; ;;;  ;;  ;;;;;   ;;;;;   ;;;;;  ;; ;;       ;;;;;;  ;;   ;;;;;  ;;;;;;  ;;;;;;  ;;;;;;  ;;;;;; 
;   ;;  ;;  ;;  ;;  ;;; ;;   ;;        ;;  ;;;;;       ;;  ;;  ;;      ;;  ;;  ;;  ;;  ;; ;;;  ;;  ;;  ;; 
;   ;;      ;;  ;;  ;;  ;;   ;;    ;;;;;;   ;;;        ;;  ;;  ;;  ;;;;;;  ;;      ;;      ;;  ;;  ;;     
;   ;;;;    ;;  ;;  ;;  ;;   ;;    ;;;;;;   ;;;        ;;      ;;  ;;;;;;  ;;;;    ;;;;    ;;;;;;  ;;;;   
;    ;;;;;  ;;  ;;  ;;  ;;   ;;    ;;  ;;   ;;;        ;;      ;;  ;;  ;;   ;;;;;   ;;;;;  ;;       ;;;;; 
;       ;;  ;; ;;;  ;;  ;;   ;;    ;;  ;;   ;;;        ;;  ;;  ;;  ;;  ;;      ;;      ;;  ;;  ;;      ;; 
;   ;;  ;;  ;; ;;;  ;;  ;;   ;;    ;;  ;;  ;; ;;       ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;; 
;   ;;;;;    ;;;;   ;;  ;;   ;;;;  ;;;;;;; ;; ;;       ;;;;;;  ;;  ;;;;;;; ;;;;;   ;;;;;   ;;;;;   ;;;;;  
;     ;;      ;;;   ;;  ;;     ;;   ;;;    ;;  ;        ;;;;   ;;   ;;;      ;;      ;;     ;;;      ;;   
;              ;;                                                                                         
;              ;;                                                                                         
;              ;;                                                                                         

(begin-for-syntax
  ; a oneshot is either a note, tuplet, or pattern
  (define-syntax-class oneshot
    (pattern (~or t:tup n:note p:pat poly:poly)))

  ; a note within a track should just be a binding to a note
  (define-syntax-class note
    (pattern n:id))

  ; a tuplet is a number of beats, followed by zero or more oneshots
  (define-syntax-class tup
    (pattern (beats:number oneshot:oneshot ...)))

  ; a polyrhythm is an expression marked by `:` consisting of a series of patterns
  (define-syntax-class poly
    (pattern ((~datum :) pat:pat ...)))
  
  ; a pattern is a series of zero or more oneshots
  (define-syntax-class pat
    (pattern (~and (~not poly:poly) (oneshot:oneshot ...))))

  ; a prim is a built-in function that can appear on the right side of a tlet bind
  (define-syntax-class prim
    (pattern (~or (~datum load) (~datum pitch) (~datum stretch) (~datum reverse) (~datum resample))))

  (define positive-rational-error "value must be positive rational number")
  
  ; Syntax -> Void
  ; recursively inspects oneshots to find if any nested tuplet contain invalid beats
  (define (check-valid-oneshot! os)
    (syntax-parse os
      [t:tup (check-positive-rational! #'t.beats)
             (map check-valid-oneshot!(syntax->list #'(t.oneshot ...)))
             (void)]
      [p:poly (map check-valid-oneshot! (syntax->list #'(p.pat ...)))
              (void)]
      [p:pat (map check-valid-oneshot! (syntax->list #'(p.oneshot ...)))
             (void)]
      [n:note (void)]))

  ; Syntax -> Void
  ; throws a syntax error if the syntax object passed doesn't represent a positive rational number
  (define (check-positive-rational! stx)
    (define value (syntax->datum stx))
    (cond
      [(not (and (real? value) (positive? value) (rational? value))) (raise-syntax-error 'tuplet positive-rational-error stx)]
      [#t (void)]))
  )

;                            ;;                                                      
;                            ;;                                                      
;    ;;;;  ;;;  ;;   ;;;;   ;;;;;  ;;;;;  ;;; ;;;       ;;;;  ;;;;;;   ;;;;    ;;;;  
;   ;;;;;; ;;;  ;;  ;;;;;   ;;;;;   ;;;;;  ;; ;;       ;;;;;;  ;;;;;; ;;;;;;  ;;;;;; 
;   ;;  ;;  ;;  ;;  ;;; ;;   ;;        ;;  ;;;;;       ;;  ;;  ;;  ;;;;;  ;;  ;;  ;; 
;   ;;      ;;  ;;  ;;  ;;   ;;    ;;;;;;   ;;;        ;;      ;;  ;; ;;  ;;  ;;  ;; 
;   ;;;;    ;;  ;;  ;;  ;;   ;;    ;;;;;;   ;;;        ;;;;    ;;  ;; ;;;;;;  ;;     
;    ;;;;;  ;;  ;;  ;;  ;;   ;;    ;;  ;;   ;;;         ;;;;;  ;;  ;; ;;      ;;     
;       ;;  ;; ;;;  ;;  ;;   ;;    ;;  ;;   ;;;            ;;  ;;  ;; ;;  ;;  ;;  ;; 
;   ;;  ;;  ;; ;;;  ;;  ;;   ;;    ;;  ;;  ;; ;;       ;;  ;;  ;;;;;; ;;  ;;  ;;  ;; 
;   ;;;;;    ;;;;   ;;  ;;   ;;;;  ;;;;;;; ;; ;;       ;;;;;   ;;;;;  ;;;;;   ;;;;;; 
;     ;;      ;;;   ;;  ;;     ;;   ;;;    ;;  ;         ;;    ;;      ;;;     ;;;;  
;              ;;                                              ;;                    
;              ;;                                              ;;                    
;              ;;                                              ;;                    

(syntax-spec
 ; track - binds a series of tuplets as a squeezed track
 ; <track> := (track [Datum] [Rational] (<tuplet>...+))
 (host-interface/definitions
  (track name:racket-var bpm:number measure:tup ...+)
  #:binding (export name)
  (check-positive-rational! #'bpm)
  (map check-valid-oneshot! (syntax->list #'(measure ...)))
  #`(define name (rt:squeeze-track (rt:track #,(symbol->string (syntax->datum #'name)) bpm (list (compile-tuplet measure) ...)))))

 ; tlet - binds a oneshot to a name
 ; <tlet> := (let <id> <expr>)
 ; <expr> := <oneshot> | (load [String] [Boolean?])
 (host-interface/definitions
  (tlet name:racket-var bind:expr)
  #:binding (export name)
  #'(define name (compile-bind bind)))
 )

;                                     ;;                     ;;                 
;                                     ;;  ;;                 ;;                 
;                                     ;;  ;;           ;;    ;;                 
;                                         ;;           ;;                       
;    ;;;;    ;;;;    ;;; ;;;  ;;;;;;  ;;  ;;  ;;;;;   ;;;;;  ;;   ;;;;    ;;;;  
;   ;;;;;;  ;;;;;;  ;;;;;;;;;  ;;;;;; ;;  ;;   ;;;;;  ;;;;;  ;;  ;;;;;;  ;;;;;  
;   ;;  ;;  ;;  ;;  ;;;;;;;;;  ;;  ;; ;;  ;;      ;;   ;;    ;;  ;;  ;;  ;;; ;; 
;   ;;  ;;  ;;  ;;  ;; ;;; ;;  ;;  ;; ;;  ;;  ;;;;;;   ;;    ;;  ;;  ;;  ;;  ;; 
;   ;;      ;;  ;;  ;; ;;; ;;  ;;  ;; ;;  ;;  ;;;;;;   ;;    ;;  ;;  ;;  ;;  ;; 
;   ;;      ;;  ;;  ;; ;;; ;;  ;;  ;; ;;  ;;  ;;  ;;   ;;    ;;  ;;  ;;  ;;  ;; 
;   ;;  ;;  ;;  ;;  ;; ;;; ;;  ;;  ;; ;;  ;;  ;;  ;;   ;;    ;;  ;;  ;;  ;;  ;; 
;   ;;  ;;  ;;  ;;  ;; ;;; ;;  ;;;;;; ;;  ;;  ;;  ;;   ;;    ;;  ;;  ;;  ;;  ;; 
;   ;;;;;;  ;;;;;   ;; ;;; ;;  ;;;;;  ;;  ;;  ;;;;;;;  ;;;;  ;;  ;;;;;   ;;  ;; 
;    ;;;;     ;;    ;;     ;;  ;;     ;;  ;;   ;;;       ;;  ;;    ;;    ;;  ;; 
;                              ;;                                               
;                              ;;                                               
;                              ;;                                               

; Syntax -> Syntax
; converts a tuplet syntax object to a runtime tuplet struct
(define-syntax compile-tuplet
  (syntax-parser
    [(_ t:tup) #'(rt:tuplet t.beats (list (compile-oneshot t.oneshot) ...))]))

; Syntax -> Syntax
; converts a pattern syntax object to a runtime pattern struct
(define-syntax compile-pattern
  (syntax-parser
    [(_ p:pat) #'(rt:pattern (list (compile-oneshot p.oneshot) ...))]))

; Syntax -> Syntax
; converts a polyrhythm syntax object to a runtime polyrhythm struct
(define-syntax compile-polyrhythm
  (syntax-parser
    [(_ p:poly) #'(rt:polyrhythm (list (compile-pattern p.pat) ...))]))

; Syntax -> Syntax
; converts a oneshot syntax object to its respective runtime struct, depending on its shape
(define-syntax compile-oneshot
  (syntax-parser
    [(_ n:note) #'n]
    [(_ t:tup) #'(compile-tuplet t)]
    [(_ p:poly) #'(compile-polyrhythm p)]
    [(_ p:pat) #'(compile-pattern p)]))

; Syntax -> Syntax
; if the bind is a known expression, returns that expression
; otherwise, compiles oneshot depending on shape
(define-syntax compile-bind
  (syntax-parser
    [(_ (op:prim arg ...)) #'(op arg ...)]
    [(_ n:note) #'n]
    [(_ t:tup) #'(compile-tuplet t)]
    [(_ p:poly) #'(compile-polyrhythm p)]
    [(_ p:pat) #'(compile-pattern p)]))

;    ;;                     ;;           
;    ;;                     ;;           
;   ;;;;;   ;;;;    ;;;;   ;;;;;   ;;;;  
;   ;;;;;  ;;;;;;  ;;;;;;  ;;;;;  ;;;;;; 
;    ;;   ;;;  ;;  ;;  ;;   ;;    ;;  ;; 
;    ;;    ;;  ;;  ;;       ;;    ;;     
;    ;;    ;;;;;;  ;;;;     ;;    ;;;;   
;    ;;    ;;       ;;;;;   ;;     ;;;;; 
;    ;;    ;;  ;;      ;;   ;;        ;; 
;    ;;    ;;  ;;  ;;  ;;   ;;    ;;  ;; 
;    ;;;;  ;;;;;   ;;;;;    ;;;;  ;;;;;  
;      ;;   ;;;      ;;       ;;    ;;

(module+ test
  (require rackunit (submod "util.rkt" test) syntax/macro-testing rsound)
  
  (begin-for-syntax
    (require rackunit syntax/parse (submod "util.rkt" test))
    
    ; syntax class matchers
    (define (syntax-note? stx)
      (syntax-parse stx
        [n:note #t]
        [_ #f]))

    (define (syntax-tup? stx)
      (syntax-parse stx
        [t:tup #t]
        [_ #f]))

    (define (syntax-poly? stx)
      (syntax-parse stx
        [p:poly #t]
        [_ #f]))

    (define (syntax-pat? stx)
      (syntax-parse stx
        [p:pat #t]
        [_ #f]))

    (define (syntax-oneshot? stx)
      (syntax-parse stx
        [os:oneshot #t]
        [_ #f]))

    (define (syntax-prim? stx)
      (syntax-parse stx
        [op:prim #t]
        [_ #f]))

    ; oneshot examples
    (define ex-note-k #'kick)
    (define ex-note-s #'snare)
    (define ex-tup-empty #'(1))
    (define ex-tup-simple #'(4 k s k s))
    (define ex-tup-fraction #'(3/2 s s s s))
    (define ex-tup-nestedtup #'(4 k s k (1 s s s)))
    (define ex-tup-nestedpat #'(4 k s (k s)))
    (define ex-tup-nestedpoly #'(4 k k (: (k k) (s s s))))
    (define ex-poly-empty #'(:))
    (define ex-poly-simple #'(: (k k)))
    (define ex-poly-multiple #'(: (k k) (s s s)))
    (define ex-poly-nested-tup #'(: (k (1 k k)) (s s s)))
    (define ex-pat-empty #'())
    (define ex-pat-simple #'(k k k k))
    (define ex-pat-nestedtup #'(k k (1 s s s) k))
    (define ex-pat-nestedpat #'(k s (k) s))
    (define ex-prim #'load)

    ; note syntax class tests
    (check-equal? (syntax-note? ex-note-k) #t)
    (check-equal? (syntax-note? ex-note-s) #t)
    (check-equal? (syntax-note? ex-pat-simple) #f)
    (check-equal? (syntax-note? ex-tup-simple) #f)
    (check-equal? (syntax-note? ex-poly-simple) #f)

    ; tup syntax class tests
    (check-equal? (syntax-tup? ex-tup-empty) #t)
    (check-equal? (syntax-tup? ex-tup-simple) #t)
    (check-equal? (syntax-tup? ex-tup-fraction) #t)
    (check-equal? (syntax-tup? ex-tup-nestedtup) #t)
    (check-equal? (syntax-tup? ex-tup-nestedpat) #t)
    (check-equal? (syntax-tup? ex-tup-nestedpoly) #t)
    (check-equal? (syntax-tup? ex-note-k) #f)
    (check-equal? (syntax-tup? ex-poly-empty) #f)
    (check-equal? (syntax-tup? ex-pat-empty) #f)
    (check-equal? (syntax-tup? ex-pat-simple) #f)

    ; pat syntax class tests
    (check-equal? (syntax-pat? ex-pat-empty) #t)
    (check-equal? (syntax-pat? ex-pat-simple) #t)
    (check-equal? (syntax-pat? ex-pat-nestedtup) #t)
    (check-equal? (syntax-pat? ex-pat-nestedpat) #t)
    (check-equal? (syntax-pat? ex-note-s) #f)
    (check-equal? (syntax-pat? ex-tup-empty) #f)
    (check-equal? (syntax-pat? ex-tup-simple) #f)
    (check-equal? (syntax-pat? ex-poly-empty) #f)
    (check-equal? (syntax-pat? ex-poly-simple) #f)

    ; poly syntax class tests
    (check-equal? (syntax-poly? ex-poly-empty) #t)
    (check-equal? (syntax-poly? ex-poly-empty) #t)
    (check-equal? (syntax-poly? ex-poly-empty) #t)
    (check-equal? (syntax-poly? ex-poly-empty) #t)
    (check-equal? (syntax-poly? ex-note-s) #f)
    (check-equal? (syntax-poly? ex-tup-simple) #f)
    (check-equal? (syntax-poly? ex-pat-simple) #f)
    (check-equal? (syntax-poly? ex-pat-empty) #f)

    ; oneshot syntax class tests
    (check-equal? (syntax-oneshot? ex-note-k) #t)
    (check-equal? (syntax-oneshot? ex-tup-fraction) #t)
    (check-equal? (syntax-oneshot? ex-tup-nestedpat) #t)
    (check-equal? (syntax-oneshot? ex-poly-nested-tup) #t)
    (check-equal? (syntax-oneshot? ex-pat-nestedtup) #t)
    (check-equal? (syntax-oneshot? #'220) #f)
    (check-equal? (syntax-oneshot? #'"hayasaka") #f)
    
    ; prim syntax class tests
    (check-equal? (syntax-prim? ex-prim) #t)
    (check-equal? (syntax-prim? #'"load") #f)
    (check-equal? (syntax-prim? ex-note-s) #f)

    ; check-positive-rational! tests
    (check-equal? (check-positive-rational! #'999/10) (void))
    (check-except (check-positive-rational! #'0) positive-rational-error)
    (check-except (check-positive-rational! #'-5) positive-rational-error)
    (check-except (check-positive-rational! #'+inf.0) positive-rational-error)
    (check-except (check-positive-rational! #'3+2i) positive-rational-error)

    ; check-valid-oneshot! tests
    (check-equal? (check-valid-oneshot! ex-note-k) (void))
    (check-equal? (check-valid-oneshot! ex-pat-simple) (void))
    (check-equal? (check-valid-oneshot! ex-pat-nestedpat) (void))
    (check-equal? (check-valid-oneshot! ex-pat-nestedtup) (void))
    (check-equal? (check-valid-oneshot! ex-tup-simple) (void))
    (check-equal? (check-valid-oneshot! ex-tup-nestedpat) (void))
    (check-equal? (check-valid-oneshot! ex-tup-nestedtup) (void))
    (check-except (check-valid-oneshot! #'(k s (-2 s s s))) positive-rational-error)
    (check-except (check-valid-oneshot! #'(0 k s k s)) positive-rational-error)
    (check-except (check-valid-oneshot! #'(: (k s) (k (-3/2 s s s)))) positive-rational-error)
    (check-except (check-valid-oneshot! #'(7 k s (-2 s s s))) positive-rational-error)
    )

  (define load rt:load) ; alias rt:load to load such that datum checks work as intended

  ; tlet examples
  (tlet k (load (sample-path "k.wav")))
  (tlet s (load (sample-path "s.wav")))
  (tlet r (load (sample-path "r.wav")))
  (tlet h (load (sample-path "h.wav") #:chop? #f))
  (tlet k2 k)
  (tlet kk (k k))
  (tlet triplet (1 s s s))
  (tlet _ (1))
  (tlet three-two (: (k k) (s s s)))
  (tlet nested ((1 k k) (1 s (1 s k))))
  
  ; track examples
  (track out_syntax_integration_1 122.5 (4 k s k s) (4 k s (1 k k) triplet))
  (track out_syntax_integration_2 180
       (4 k r s r)
       (4 three-two three-two)
       (4 k (r (2 s _ r _)))
       (4 s s s s s s s s)
       (4 (1 k k) (1 s (1 s k)) (1 r r k k) (1 s (1 s h))))

  ; track output examples
  (define ex-track-integration-1-out (rt:track "out_exp_syntax_integration_1" 122.5 (list (rt:tuplet 4 (list k s k s)) (rt:tuplet 4 (list k s (rt:tuplet 1 (list k k)) triplet)))))
  (define ex-track-integration-2-out (rt:track "out_exp_syntax_integration_2" 180
                                            (list
                                             (rt:tuplet 4 (list k r s r))
                                             (rt:tuplet 4 (list three-two three-two))
                                             (rt:tuplet 4 (list k (rt:pattern (list r (rt:tuplet 2 (list s _ r _))))))
                                             (rt:tuplet 4 (list s s s s s s s s))
                                             (rt:tuplet 4 (list (rt:tuplet 1 (list k k)) (rt:tuplet 1 (list s (rt:tuplet 1 (list s k)))) (rt:tuplet 1 (list r r k k)) (rt:tuplet 1 (list s (rt:tuplet 1 (list s h)))))))))
  
  ; tlet syntax spec tests
  (check-syntax-except (tlet 88 (load ".")) "expected identifier")
  (check-syntax-except (tlet name #'"not a tuplet expr") "not an expression")
  
  ; track syntax spec tests
  (check-syntax-except (track 99 99 (4 k s k s)) "expected identifier")
  (check-syntax-except (track invalid_bpm "sdf" (4 k s k s)) "expected number")
  (check-syntax-except (track invalid_bpm -5 (4 k s k s)) "positive rational number")
  (check-syntax-except (track measure_not_tup 140 (k s k s)) "while parsing tup")
  (check-syntax-except (track measure_not_tup 140 (-5 k s k s)) "positive rational number")

  ; integration
  (rt:save! out_syntax_integration_1 (test-file-path))
  (rt:save! (rt:squeeze-track ex-track-integration-1-out) (test-file-path))
  (check-equal? (rs-read (test-file-path "out_syntax_integration_1.wav")) (rs-read (test-file-path "out_exp_syntax_integration_1.wav")))
  (rt:save! out_syntax_integration_2 (test-file-path))
  (rt:save! (rt:squeeze-track ex-track-integration-2-out) (test-file-path))
  (check-equal? (rs-read (test-file-path "out_syntax_integration_2.wav")) (rs-read (test-file-path "out_exp_syntax_integration_2.wav")))
  )
