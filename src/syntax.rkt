#lang racket

(require (prefix-in rt: "runtime.rkt") (prefix-in rsound: rsound) syntax-spec-v3 (for-syntax syntax/parse racket/syntax))

(provide track (rename-out [tlet let]))

(begin-for-syntax
  ; a oneshot is either a note, tuplet, or pattern
  (define-syntax-class oneshot
    (pattern (~or t:tup n:note p:pat)))

  ; a note within a track should just be a binding to a note
  (define-syntax-class note
    (pattern n:id))

  ; a tuplet is a number of beats, followed by zero or more oneshots
  (define-syntax-class tup
    (pattern (beats:number oneshot:oneshot ...)))

  ; a pattern is a series of zero or more oneshots
  (define-syntax-class pat
    (pattern (oneshot:oneshot ...)))

  ; a prim is a built-in function that can appear on the right side of a tlet bind
  (define-syntax-class prim
    (pattern (~or (~datum load))))

  ; Syntax -> Void
  ; recursively inspects oneshots to find if any nested tuplet contain invalid beats
  (define (check-valid-oneshot! os)
    (syntax-parse os
      [t:tup (check-positive-rational! #'t.beats)
             (map check-valid-oneshot!(syntax->list #'(t.oneshot ...)))]
      [p:pat (map check-valid-oneshot! (syntax->list #'(p.oneshot ...)))]
      [n:note (void)]))

  ; Syntax -> Void
  ; throws a syntax error if the syntax object passed doesn't represent a positive rational number
  (define (check-positive-rational! stx)
    (define value (syntax->datum stx))
    (cond
      [(not (and (positive? value) (rational? value))) (raise-syntax-error 'tuplet "value must be positive rational number" stx)]
      [#t (void)]))
  )

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

; Syntax -> Syntax
; converts a tuplet syntax object to a runtime tuplet struct
; there are some complications involving handling #%host-expression
(define-syntax compile-tuplet
  (syntax-parser
    [(_ t:tup) #'(rt:tuplet t.beats (list (compile-oneshot t.oneshot) ...))]))

; Syntax -> Syntax
; converts a pattern syntax object to a runtime pattern struct
; there are some complications involving handling #%host-expression
(define-syntax compile-pattern
  (syntax-parser
    [(_ p:pat) #'(rt:pattern (list (compile-oneshot p.oneshot) ...))]))

; Syntax -> Syntax
; converts a oneshot syntax object to its respective runtime struct, depending on its shape
; there are some complications involving handling #%host-expression
(define-syntax compile-oneshot
  (syntax-parser
    [(_ n:note) #'n]
    [(_ t:tup) #'(compile-tuplet t)]
    [(_ p:pat) #'(compile-pattern p)]))

; Syntax -> Syntax
; if the bind is a known expression, returns that expression
; otherwise, compiles oneshot depending on shape
(define-syntax compile-bind
  (syntax-parser
    [(_ (op:prim arg ...))
     (define/with-syntax fmt-op (format-symbol "rt:~a" #'op))
     #'(fmt-op arg ...)]
    [(_  n:note) #'n]
    [(_ t:tup) #'(compile-tuplet t)]
    [(_ p:pat) #'(compile-pattern p)]))