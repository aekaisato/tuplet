#lang racket

(require rsound)
(provide (all-defined-out))

;       ;;                                  ;;             ;;;        
;       ;;                                  ;;            ;;;;        
;       ;;           ;;                     ;;           ;;;;         
;       ;;           ;;                     ;;           ;;           
;    ;;;;;  ;;;;;   ;;;;;  ;;;;;         ;;;;;   ;;;;    ;;     ;;;;  
;   ;;;;;;   ;;;;;  ;;;;;   ;;;;;       ;;;;;;  ;;;;;;   ;;;;  ;;;;;; 
;  ;;;  ;;      ;;   ;;        ;;      ;;;  ;; ;;;  ;;  ;;;;;  ;;  ;; 
;   ;;  ;;  ;;;;;;   ;;    ;;;;;;       ;;  ;;  ;;  ;;  ;;;;;  ;;     
;   ;;  ;;  ;;;;;;   ;;    ;;;;;;       ;;  ;;  ;;;;;;   ;;    ;;;;   
;   ;;  ;;  ;;  ;;   ;;    ;;  ;;       ;;  ;;  ;;       ;;     ;;;;; 
;   ;;  ;;  ;;  ;;   ;;    ;;  ;;       ;;  ;;  ;;  ;;   ;;        ;; 
;   ;;  ;;  ;;  ;;   ;;    ;;  ;;       ;;  ;;  ;;  ;;   ;;    ;;  ;; 
;   ;;;;;;  ;;;;;;;  ;;;;  ;;;;;;;      ;;;;;;  ;;;;;    ;;    ;;;;;  
;    ;;;;    ;;;       ;;   ;;;          ;;;;    ;;;     ;;      ;;   

; a oneshot is either a note, tuplet, or pattern
(define/contract (oneshot? x) (-> any/c boolean?) (or (tuplet? x) (note? x) (placeholder? x) (pattern? x) (polyrhythm? x)))

; represents a single sound that can be played
(define-struct/contract note ([sound rsound?] [chop? boolean?]))

; placeholder for a note (or getting in-out information without a sound)
(define-struct/contract placeholder ([name symbol?]))

; represents a series of oneshots that "squeeze" to fit into the given number of beats
(define-struct/contract tuplet ([beats (and/c positive? rational?)] [contents (listof oneshot?)]))

; represents a series of oneshots that don't "squeeze" to fit into the given number of beats
(define-struct/contract pattern ([contents (listof oneshot?)]))

; represents a series of patterns that will all play at once to create polyrhthmic sequences
(define-struct/contract polyrhythm ([contents (listof pattern?)]))

; represents a full audio track
(define-struct/contract track ([name string?] [bpm (and/c positive? rational?)] [measures (non-empty-listof tuplet?)]))

; intermediary format for notes that includes the sample to start playing and the sample to stop playing
(define-struct/contract note-assembly ([sound rsound?] [in-sample (and/c (not/c negative?) rational?)] [chop? boolean?] [chop-sample (and/c (not/c negative?) rational?)]) #:transparent)

; output for placeholder
(define-struct/contract placeholder-assembly ([name symbol?] [in-sample (and/c (not/c negative?) rational?)] [chop-sample (and/c (not/c negative?) rational?)]) #:transparent)

; intermediate format for tracks intended to map to input for rsound's assemble
(define-struct/contract track-assembly ([name string?] [assembly (non-empty-listof note-assembly?)]) #:transparent)

; check if oneshot contains a placeholder or not
(define/contract (contains-placeholder? os)
  (-> oneshot? boolean?)
  (match os
    [(note _ _) #f]
    [(placeholder _) #t]
    [(tuplet _ contents) (ormap contains-placeholder? contents)]
    [(pattern contents) (ormap contains-placeholder? contents)]
    [(polyrhythm contents) (ormap contains-placeholder? contents)]))

(define squeeze-output? (or/c (listof note-assembly?) (listof placeholder-assembly?)))

;                                                               ;;                       
;                                                               ;;                    ;; 
;                                                               ;;                    ;; 
;                                                                                     ;; 
;    ;;;;    ;;;;; ;;  ;;;  ;;;;    ;;;;   ;;;;;;   ;;;;        ;;   ;;; ;;;  ;;;;;;  ;; 
;   ;;;;;;  ;;;;;; ;;  ;;; ;;;;;;  ;;;;;;  ;;;;;;  ;;;;;;       ;;  ;;;;;;;;;  ;;;;;; ;; 
;   ;;  ;; ;;;  ;; ;;  ;;;;;;  ;; ;;;  ;;      ;; ;;;  ;;       ;;  ;;;;;;;;;  ;;  ;; ;; 
;   ;;     ;;;  ;; ;;  ;;  ;;  ;;  ;;  ;;     ;;   ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;;;   ;;;  ;; ;;  ;;  ;;;;;;  ;;;;;;    ;;    ;;;;;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;;;;  ;;  ;; ;;  ;;  ;;      ;;       ;;;    ;;           ;;  ;; ;;; ;;  ;;  ;; ;; 
;       ;;  ;;  ;; ;;  ;;  ;;  ;;  ;;  ;;  ;;;     ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;  ;;  ;;;;;; ;;;;;;  ;;  ;;  ;;  ;; ;;;;;;;  ;;  ;;       ;;  ;; ;;; ;;  ;;;;;; ;; 
;   ;;;;;    ;;;;;  ;;;;;; ;;;;;   ;;;;;   ;;;;;;  ;;;;;        ;;  ;; ;;; ;;  ;;;;;  ;; 
;     ;;       ;;;   ;;  ;  ;;;     ;;;    ;;;;;;   ;;;         ;;  ;;     ;;  ;;     ;; 
;               ;;                                                             ;;        
;               ;;                                                             ;;        
;              ;;;                                                             ;;        

; get the number of beats a list of oneshots represents
(define/contract (rhythm-size oneshot-list)
  (-> (listof oneshot?) integer?)
  (match oneshot-list
    ['() 0]
    [(cons (or (note _ _) (placeholder _)) rest) (+ 1 (rhythm-size rest))]
    [(cons (tuplet beats _) rest) (+ beats (rhythm-size rest))]
    [(cons (pattern contents) rest) (+ (rhythm-size contents) (rhythm-size rest))]
    [(cons (polyrhythm ptrns) rest) (+ 1 (rhythm-size rest))]))

; squeezes a list of oneshots by placing notes at in-out points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-helper ls starting-sample samples-per-beat)
  (-> (listof oneshot?) (not/c negative?) (not/c negative?) squeeze-output?)
  (match ls
    ['() '()]
    [(cons this rest)
     (match this
       [(note sound chop?)
        (define next-beat (+ starting-sample samples-per-beat))
        (cons (note-assembly sound starting-sample chop? next-beat)
              (squeeze-helper rest next-beat samples-per-beat))]
       [(placeholder name)
        (define next-beat (+ starting-sample samples-per-beat))
        (cons (placeholder-assembly name starting-sample next-beat)
              (squeeze-helper rest next-beat samples-per-beat))]
       [(tuplet beats contents)
        (define next-beat (+ starting-sample (* samples-per-beat beats)))
        (append (squeeze-tuplet this starting-sample samples-per-beat)
                (squeeze-helper rest next-beat samples-per-beat))]
       [(pattern contents)
        (define next-beat (+ starting-sample (* samples-per-beat (rhythm-size contents))))
        (append (squeeze-pattern this starting-sample samples-per-beat)
                (squeeze-helper rest next-beat samples-per-beat))]
       [(polyrhythm contents)
        (define next-beat (+ starting-sample samples-per-beat))
        (append (squeeze-polyrhythm this starting-sample samples-per-beat)
                (squeeze-helper rest next-beat samples-per-beat))])]))

; squeezes a tuplet by placing notes at in points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-tuplet tup starting-sample samples-per-beat)
  (-> tuplet? (not/c negative?) (not/c negative?) squeeze-output?)
  (match tup
    [(tuplet tup-beats tup-contents)
     (define size (rhythm-size tup-contents))
     (cond [(= size 0) '()]
           [else (define ratio (/ tup-beats size))
                 (squeeze-helper tup-contents starting-sample (* samples-per-beat ratio))])]))

; places in points for notes contained in a pattern
(define/contract (squeeze-pattern pat starting-sample samples-per-beat)
  (-> pattern? (not/c negative?) (not/c negative?) squeeze-output?)
  (match pat
    [(pattern contents)
     (squeeze-helper contents starting-sample samples-per-beat)]))

; places in points for notes contained in a polyrhythm
(define/contract (squeeze-polyrhythm poly starting-sample samples-per-beat)
  (-> polyrhythm? (not/c negative?) (not/c negative?) squeeze-output?)
  (match poly
    [(polyrhythm ptrns)
     (flatten (map (lambda (pat)
                     (squeeze-pattern
                      pat
                      starting-sample
                      (/ samples-per-beat (rhythm-size (pattern-contents pat)))))
                   ptrns))]))