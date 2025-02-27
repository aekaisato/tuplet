#lang racket

(require rsound)

(define/contract (oneshot? x) (-> any/c boolean?) (or (tuplet? x) (note? x)))

(define-struct/contract note ([sound rsound?] [chop? boolean?]))

(define-struct/contract tuplet ([beats rational?] [contents (listof oneshot?)]))

(define-struct/contract track ([name string?] [bpm rational?] [measures (listof tuplet?)]))

(define-struct/contract note-assembly ([sound rsound?] [in-sample (not/c negative?)] [out-sample (or (not/c negative?) false?)]))

(define-struct/contract track-assembly ([assembly (listof note-assembly?)]))

(define/contract (bpm-to-samples bpm)
  (-> rational? nonnegative-integer?)
  (define sample-rate (default-sample-rate))
  (round (* (* (/ 1 bpm) 60) sample-rate)))

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

(define/contract (squeeze-oneshot-list ls starting-sample samples-per-beat)
  (-> (listof oneshot?) (not/c negative?) (not/c negative?) (listof note-assembly?))
  (match ls
    ['() '()]
    [(cons (note sound chop?) rest)
     (define next-beat (+ starting-sample samples-per-beat))
     (cons (note-assembly sound starting-sample (if chop? next-beat #f))
           (squeeze-oneshot-list rest next-beat samples-per-beat))]
    [(cons (tuplet beats contents) rest)
     (define next-beat (+ starting-sample (* samples-per-beat beats)))
     (define tup-assembly (squeeze-tuplet (tuplet beats contents) starting-sample (/ samples-per-beat beats)))
     (append tup-assembly (squeeze-oneshot-list rest next-beat samples-per-beat))]))

(define/contract (squeeze-tuplet tup starting-sample samples-per-beat)
  (-> tuplet? (not/c negative?) (not/c negative?) (listof note-assembly?))
  (define tup-beats (tuplet-beats tup))
  (define tup-contents (tuplet-contents tup))
  (define ratio (/ tup-beats (length tup-contents)))
  (squeeze-oneshot-list tup-contents starting-sample (* samples-per-beat ratio)))

(define/contract (assemble-track assembly)
  (-> track-assembly? rsound?)
  (define asm (track-assembly-assembly assembly))
  (define rs-asm (map (lambda (note-asm)
         (define s (note-assembly-sound note-asm))
         (define in (round (note-assembly-in-sample note-asm)))
         (define out (round (note-assembly-out-sample note-asm)))
         (define rs (if out (clip s 0 (min (- out in) (rs-frames s))) s))
         (list rs in)) asm))
  (assemble rs-asm))

(define k (note (rs-read (normalize-path "../examples/samples/k.wav")) #t))
(define s (note (rs-read (normalize-path "../examples/samples/s.wav")) #t))
(define r (note (rs-read (normalize-path "../examples/samples/r.wav")) #t))
(define h (note (rs-read (normalize-path "../examples/samples/h.wav")) #t))

(define tracktest
  (track "test1" 180
         (list
          (tuplet 4 (list k r s r))
          (tuplet 4 (list k s (tuplet 1 (list k k)) h))
          (tuplet 4 (list k r s r))
          (tuplet 4 (list s s s s s s s s))
          (tuplet 4 (list (tuplet 1 (list k k)) (tuplet 1 (list s (tuplet 1 (list s k)))) (tuplet 1 (list r r k k)) (tuplet 1 (list s (tuplet 1 (list s h))))))
          )))

(define tracktest-asm (squeeze-track tracktest))

(define assembled (assemble-track tracktest-asm))

(play assembled)