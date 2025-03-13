#lang racket

(require (prefix-in rt: "runtime.rkt") (prefix-in rsound: rsound) syntax-spec-v3 (for-syntax syntax/parse))

(provide track (rename-out [tlet let] [rt:load load] [rt:save! save!] [rt:play! play!]))

(syntax-spec
 ; track - binds a series of tuplets as a squeezed track
 ; <track> := (track [Datum] [Rational] (<tuplet>...+))
 (host-interface/definitions
  (track name:racket-var bpm:racket-expr measure:tuplet-spec ...)
  #:binding (export name)
  #`(define name (rt:squeeze-track (rt:track #,(symbol->string (syntax->datum #'name)) bpm (list (compile-tuplet measure) ...)))))

 ; tlet - binds a oneshot to a name
 ; <tlet> := (let <id> <expr>)
 ; <expr> := <oneshot> | (load [String] [Boolean?])
 (host-interface/definitions
  (tlet name:racket-var bind:bind-spec)
  #:binding (export name)
  #'(define name (compile-bind bind)))

 ; a bind is either a oneshot (a tuplet, pattern, or note), or an arbitrary expression (that should eval to a oneshot)
 (nonterminal bind-spec
              expr:racket-expr)

 ; a tuplet is a number of beats, followed by zero or more oneshots
 (nonterminal tuplet-spec
              (beats:number oneshot:racket-expr ...))

 ; a pattern is a series of zero or more oneshots
 (nonterminal pattern-spec
              (oneshot:racket-expr ...))

 ; a note within a track should just be a binding to a note
 (nonterminal note-spec
              note:racket-var)
 )

; Syntax -> Syntax
; converts a tuplet syntax object to a runtime tuplet struct
; there are some complications involving handling #%host-expression
(define-syntax compile-tuplet
  (syntax-parser
    [(_ (beats oneshot ...)) #'(rt:tuplet beats (list (compile-oneshot oneshot) ...))]
    [(_ (#%host-expression (beats:number oneshot ...))) #'(rt:tuplet beats (list (compile-oneshot oneshot) ...))]
    [(_ ((#%host-expression beats:number) oneshot ...)) #'(rt:tuplet beats (list (compile-oneshot oneshot) ...))]))

; Syntax -> Syntax
; converts a pattern syntax object to a runtime pattern struct
; there are some complications involving handling #%host-expression
(define-syntax compile-pattern
  (syntax-parser
    [(_ (oneshot ...)) #'(rt:pattern (list (compile-oneshot oneshot) ...))]))

; Syntax -> Syntax
; converts a oneshot syntax object to its respective runtime struct, depending on its shape
; there are some complications involving handling #%host-expression
(define-syntax compile-oneshot
  (syntax-parser
    #:datum-literals (#%host-expression)
    [(_ (#%host-expression note:id)) #'note]
    [(_ note:id) #'note]
    [(_ ((#%host-expression beats:number) oneshot ...)) #'(compile-tuplet (beats oneshot ...))]
    [(_ (beats:number oneshot ...)) #'(compile-tuplet (beats oneshot ...))]
    [(_ (#%host-expression (beats:number oneshot ...))) #'(compile-tuplet (beats oneshot ...))]
    [(_ (#%host-expression (oneshot ...))) #'(compile-pattern (oneshot ...))]
    [(_ (oneshot ...)) #'(compile-pattern (oneshot ...))]))

; Syntax -> Syntax
; if the bind is a known expression, returns that expression
; otherwise, compiles oneshot depending on shape
(define-syntax compile-bind
  (syntax-parser
    #:datum-literals (#%host-expression load)
    [(_ (#%host-expression (load arg ...))) #'(rt:load arg ...)]
    [(_ (#%host-expression note:id)) #'note]
    [(_ (#%host-expression (beats:number oneshot ...))) #'(compile-tuplet (beats oneshot ...))]
    [(_ (#%host-expression (oneshot ...))) #'(compile-pattern (oneshot ...))]))