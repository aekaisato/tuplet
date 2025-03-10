#lang racket

(require rsound)

(provide oneshot? note note? tuplet tuplet? pattern pattern? track track? squeeze-track assemble-track)

(define/contract (oneshot? x) (-> any/c boolean?) (or (tuplet? x) (note? x) (pattern? x)))

; represents a single sound that can be played
(define-struct/contract note ([sound rsound?] [chop? boolean?]))

; represents a series of oneshots that "squeeze" to fit into the given number of beats
(define-struct/contract tuplet ([beats (and/c (not/c negative?) rational?)] [contents (listof oneshot?)]))

; represents a series of oneshots that don't "squeeze" to fit into the given number of beats
(define-struct/contract pattern ([contents (listof oneshot?)]))

; represents a full audio track
(define-struct/contract track ([name string?] [bpm (and/c positive? rational?)] [measures (listof tuplet?)]))

; intermediary format for notes that includes the sample to start playing and the sample to stop playing
(define-struct/contract note-assembly ([sound rsound?] [in-sample (not/c negative?)] [out-sample (not/c negative?)]))

; intermediate format for tracks intended to map to input for rsound's assemble
(define-struct/contract track-assembly ([assembly (listof note-assembly?)]))

; convert a number from beats-per-minute to samples-per-beat
(define/contract (bpm-to-samples bpm)
  (-> (and/c positive? rational?) nonnegative-integer?)
  (define sample-rate (default-sample-rate))
  (round (* (* (/ 1 bpm) 60) sample-rate)))

(define/contract (rhythm-size oneshot-list)
  (-> (listof oneshot?) integer?)
  (match oneshot-list
    ['() 0]
    [(cons (note _ _) rest) (+ 1 (rhythm-size rest))]
    [(cons (tuplet beats _) rest) (+ beats (rhythm-size rest))]
    [(cons (pattern contents) rest) (+ (rhythm-size contents) (rhythm-size rest))]))

; squeezes a track by placing notes at in-out points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-track track)
  (-> track? track-assembly?)
  (define samples-per-beat (bpm-to-samples (track-bpm track)))
  (define (helper measures starting-sample)
    (match measures
      ['() '()]
      [(cons (tuplet beats contents) rest)
       (cons (squeeze-tuplet (tuplet beats contents) starting-sample samples-per-beat)
             (helper rest (+ starting-sample (* samples-per-beat beats))))]))
  (define assembly (flatten (helper (track-measures track) 0)))
  (track-assembly assembly))

; squeezes a list of oneshots by placing notes at in-out points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-oneshot-list ls starting-sample samples-per-beat)
  (-> (listof oneshot?) (not/c negative?) (not/c negative?) (listof note-assembly?))
  (match ls
    ['() '()]
    [(cons (note sound chop?) rest)
     (define next-beat (+ starting-sample samples-per-beat))
     (cons (note-assembly sound starting-sample (if chop? next-beat (+ starting-sample (rs-frames sound))))
           (squeeze-oneshot-list rest next-beat samples-per-beat))]
    [(cons (tuplet beats contents) rest)
     (define next-beat (+ starting-sample (* samples-per-beat beats)))
     (define tup-assembly (squeeze-tuplet (tuplet beats contents) starting-sample samples-per-beat))
     (append tup-assembly (squeeze-oneshot-list rest next-beat samples-per-beat))]
    [(cons (pattern contents) rest)
     (define next-beat (+ starting-sample (* samples-per-beat (rhythm-size contents))))
     (append (squeeze-pattern (pattern contents) starting-sample samples-per-beat) (squeeze-oneshot-list rest next-beat samples-per-beat))]))

; squeezes a tuplet by placing notes at in-out points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-tuplet tup starting-sample samples-per-beat)
  (-> tuplet? (not/c negative?) (not/c negative?) (listof note-assembly?))
  (define tup-beats (tuplet-beats tup))
  (define tup-contents (tuplet-contents tup))
  (define size (rhythm-size tup-contents))
  (cond [(= size 0) '()]
        [else (define ratio (/ tup-beats size))
              (squeeze-oneshot-list tup-contents starting-sample (* samples-per-beat ratio))]))

; places in-out points for notes contained in a pattern
(define/contract (squeeze-pattern pat starting-sample samples-per-beat)
  (-> pattern? (not/c negative?) (not/c negative?) (listof note-assembly?))
  (squeeze-oneshot-list (pattern-contents pat) starting-sample samples-per-beat))

; assembles a track-assembly to a single rsound that can be played or exported as audio
(define/contract (assemble-track assembly)
  (-> track-assembly? rsound?)
  (define asm (track-assembly-assembly assembly))
  (define rs-asm (map (lambda (note-asm)
                        (define s (note-assembly-sound note-asm))
                        (define s-len (rs-frames s))
                        (define in (round (note-assembly-in-sample note-asm)))
                        (define out (round (note-assembly-out-sample note-asm)))
                        (define rs (clip s 0 (min (- out in) s-len)))
                        (list rs in)) asm))
  (assemble rs-asm))