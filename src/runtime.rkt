#lang racket

(require rsound "ffmpeg-filter.rkt" ffi/vector "squeeze.rkt")

(provide oneshot? note note? tuplet tuplet? pattern pattern? polyrhythm polyrhythm? track track? squeeze-track assemble-track load pitch stretch note-reverse resample set-chop play! save!)

;                                ;;                          ;;                       
;                                ;;                          ;;                    ;; 
;                          ;;    ;;                          ;;                    ;; 
;                          ;;                                                      ;; 
;   ;; ;; ;;  ;;;  ;;;;   ;;;;;  ;;   ;;; ;;;    ;;;;        ;;   ;;; ;;;  ;;;;;;  ;; 
;   ;;;;; ;;  ;;; ;;;;;   ;;;;;  ;;  ;;;;;;;;;  ;;;;;;       ;;  ;;;;;;;;;  ;;;;;; ;; 
;   ;;; ; ;;  ;;; ;;; ;;   ;;    ;;  ;;;;;;;;; ;;;  ;;       ;;  ;;;;;;;;;  ;;  ;; ;; 
;   ;;    ;;  ;;  ;;  ;;   ;;    ;;  ;; ;;; ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;    ;;  ;;  ;;  ;;   ;;    ;;  ;; ;;; ;;  ;;;;;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;    ;;  ;;  ;;  ;;   ;;    ;;  ;; ;;; ;;  ;;           ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;    ;;  ;;  ;;  ;;   ;;    ;;  ;; ;;; ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;    ;;;;;;  ;;  ;;   ;;    ;;  ;; ;;; ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;;;;; ;; 
;   ;;     ;;;;;; ;;  ;;   ;;;;  ;;  ;; ;;; ;;  ;;;;;        ;;  ;; ;;; ;;  ;;;;;  ;; 
;   ;;      ;;  ; ;;  ;;     ;;  ;;  ;;     ;;   ;;;         ;;  ;;     ;;  ;;     ;; 
;                                                                           ;;        
;                                                                           ;;        
;                                                                           ;;        

; options to use for any usage of rubberband for audio stretching
(define rubberband-shared-opts
  (list
   (ff-filter-option "pitchq" "quality")
   (ff-filter-option "transients" "crisp")
   (ff-filter-option "detector" "percussive")
   (ff-filter-option "window" "short")
   (ff-filter-option "smoothing" "off")
   (ff-filter-option "formant" "preserved")))

; convert a number from beats-per-minute to samples-per-beat
(define/contract (bpm-to-samples bpm)
  (-> (and/c positive? rational?) exact-nonnegative-integer?)
  (define sample-rate (default-sample-rate))
  (inexact->exact (round (* (* (/ 1 bpm) 60) sample-rate))))

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
  (track-assembly (track-name track) assembly))

; squeezes a list of oneshots by placing notes at in-out points defined by structure, an offset, and the current length of a beat
(define/contract (squeeze-oneshot-list ls starting-sample samples-per-beat)
  (-> (and/c (listof oneshot?) (not/c contains-placeholder?)) (not/c negative?) (not/c negative?) (listof note-assembly?))
  (squeeze-helper ls starting-sample samples-per-beat))

; assembles a track-assembly to a single rsound that can be played or exported as audio
(define/contract (assemble-track assembly)
  (-> track-assembly? rsound?)
  (define asm (sort (track-assembly-assembly assembly) (lambda (a b) (< (note-assembly-in-sample a) (note-assembly-in-sample b)))))
  (define rs-asm (map (lambda (note-asm i)
                        (define s (note-assembly-sound note-asm))
                        (define s-len (rs-frames s))
                        (define chop? (note-assembly-chop? note-asm))
                        (define in (round (note-assembly-in-sample note-asm)))
                        (define out (round (if chop?
                                               (if (< i (- (length asm) 1))
                                                   (note-assembly-in-sample (list-ref asm (+ i 1)))
                                                   (note-assembly-chop-sample note-asm))
                                               (+ in s-len))))
                        (define rs (clip s 0 (min (- out in) s-len)))
                        (list rs in))
                      asm (build-list (length asm) values)))
  (assemble rs-asm))

; loads a wav file based on the given filepath
; if provided, numbers represent the in and out points, in samples, where the number represents the in point if only one is provided (defaults to no clipping)
; if provided, boolean represents whether to chop off the end of the sound at the end of the beat (defaults to true)
(define/contract (load path #:in [in #f] #:out [out #f] #:chop? [chop? #t])
  (->* (path-string?) (#:in (or/c nonnegative-integer? boolean?) #:out (or/c nonnegative-integer? boolean?) #:chop? boolean?) note?)
  (define normpath (normalize-path path))
  (cond
    [(and in out) (note (rs-read/clip normpath in out) chop?)]
    [in (define rs (rs-read normpath))
        (note (clip rs in (rs-frames rs)) chop?)]
    [out (note (rs-read/clip normpath 0 out) chop?)]
    [#t (note (rs-read normpath) chop?)]))

; pitch a note up/down by the given number of semitones and cents, following 12-tone equal temperament
(define/contract (pitch n semitones [cents 0])
  (->* (note? rational?) (rational?) note?)
  (define total-cents (+ (* 100 semitones) cents))
  (define exponent (/ total-cents 1200))
  (define pitch-factor (expt 2 exponent))
  (define filters (construct-rubberband-filters 1 pitch-factor))
  (apply-filters-to-note n filters))

; stretch a note's speed by the given factor, without changing its pitch
(define/contract (stretch n factor)
  (-> note? (and/c rational? positive?) note?)
  (define filters (construct-rubberband-filters factor 1))
  (apply-filters-to-note n filters))

; given factors for tempo and pitch, construct the filters to pass to ffmpeg to perform the stretch
(define/contract (construct-rubberband-filters tempo pitch)
  (-> rational? rational? (listof ff-filter?))
  (list
   (ff-filter
    "rubberband"
    (append
     (list (ff-filter-option "tempo" (~a tempo)) (ff-filter-option "pitch" (~a pitch)))
     rubberband-shared-opts))))

; uses ffmpeg to apply a list of filters to a single note
(define/contract (apply-filters-to-note n filters)
  (-> note? (listof ff-filter?) note?)
  (match n
    [(note sound chop?) (note (apply-ffmpeg-filters sound filters) chop?)]))

; reverses the given note, preserving channels
(define/contract (note-reverse n)
  (-> note? note?)
  (match n
    [(note (rsound data start end frame-rate) chop?)
     (match-let* ([orig-list (s16vector->list data)]
                  [(list left right) (split-and-reverse orig-list (s16vector-length data))]
                  [revlist (interleave left right)])
       (note (rsound (list->s16vector revlist) start end frame-rate) chop?))]))

; split a list into two lists, each of which are reversed from their order in the original list
; a length (assumed correct) can be passed to avoid calculating
(define (split-and-reverse ls [len (length ls)])
  (->* ((listof any/c)) (nonnegative-integer?) (listof (listof any/c)))
  (foldl
   (lambda (cur i acc)
     (match acc
       [(list left right)
        (if (even? i)
            (list (cons cur left) right)
            (list left (cons cur right)))]))
   (list (list) (list))
   ls (build-list len values)))

; interleave two lists into a single list that alternates between the two, starting with ls1
(define/contract (interleave ls1 ls2)
  (-> (listof any/c) (listof any/c) (listof any/c))
  (match (list ls1 ls2)
    [(list (cons a rest1) (cons b rest2)) (cons a (cons b (interleave rest1 rest2)))]
    [(list (list) ls2) ls2]
    [(list ls1 (list)) ls1]))

; stretch a note's speed by the given factor, not correcting for pitch
(define/contract (resample n factor)
  (-> note? (and/c rational? positive?) note?)
  (match n
    [(note sound chop?) (note (resample/interp factor sound) chop?)]))

; set the chop? option on the note
(define/contract (set-chop n chop?)
  (-> note? boolean? note?)
  (match n
    [(note sound _) (note sound chop?)]))

; plays the track output through the default audio device
(define/contract (play! . tracks)
  (-> track-assembly? ... void?)
  (define num-tracks (length tracks))
  (define overlaid-tracks (rs-overlay*
                           (map (lambda (t)
                                  (rs-scale (/ 1 num-tracks) (assemble-track t)))
                                tracks)))
  (play overlaid-tracks)
  (void))

; saves a wav file containing the track output audio at the specified path (defaults to cwd)
(define/contract (save! track [path "./"])
  (->* (track-assembly?) (path-string?) void?)
  (define name (track-assembly-name track))
  (define normpath (normalize-path path))
  (define type (file-or-directory-type normpath))
  (define fullpath
    (match type
      [(or 'directory 'directory-link) (build-path normpath (string-append name ".wav"))]
      [_ normpath]))
  (rs-write (assemble-track track) fullpath))

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
  (require rackunit (for-syntax syntax/parse) (submod "util.rkt" test))

  ; oneshot examples
  (define ex-rsound-1 (make-tone 440 0.2 1000))
  (define ex-rsound-2 (make-tone 880 0.2 1000))
  (define ex-note-simple (note ex-rsound-1 #t))
  (define ex-note-nochop (note ex-rsound-2 #f))
  (define ex-tuplet-simple (tuplet 7 '()))
  (define ex-pattern-simple (pattern '()))
  (define ex-polyrhythm-simple (polyrhythm '()))
  (define ex-tuplet-notecontents (tuplet 4 (list ex-note-simple ex-note-nochop)))
  (define ex-pattern-notecontents (pattern (list ex-note-simple ex-note-nochop)))
  (define ex-polyrhythm-desync (polyrhythm (list ex-pattern-notecontents (pattern (list ex-note-simple ex-note-simple ex-note-simple)))))
  (define ex-polyrhythm-sync (polyrhythm (list ex-pattern-notecontents (pattern (list ex-note-nochop ex-note-simple)))))
  (define ex-tuplet-patterncontents (tuplet 3 (list ex-pattern-notecontents ex-pattern-simple)))
  (define ex-pattern-patterncontents (pattern (list ex-pattern-notecontents ex-pattern-simple)))
  (define ex-tuplet-tupletcontents (tuplet 5 (list ex-tuplet-notecontents ex-tuplet-simple)))
  (define ex-pattern-tupletcontents (pattern (list ex-tuplet-notecontents ex-tuplet-simple)))
  (define ex-tuplet-polyrhythmcontents (tuplet 3 (list ex-polyrhythm-desync)))
  (define ex-pattern-polyrhythmcontents (pattern (list ex-polyrhythm-desync)))
  (define ex-tuplet-ratbeats (tuplet 3/2 (list ex-note-simple ex-note-simple ex-note-simple ex-note-simple)))
  (define ex-tuplet-nested (tuplet 7 (list ex-tuplet-tupletcontents ex-pattern-tupletcontents ex-tuplet-patterncontents)))
  (define ex-pattern-nested (pattern (list ex-tuplet-tupletcontents ex-pattern-tupletcontents ex-tuplet-patterncontents)))
  (define ex-polyrhythm-nested (polyrhythm (list ex-pattern-tupletcontents ex-pattern-patterncontents)))

  ; track examples
  (define ex-track-onemeasure-simple (track "out_simple_track" 100 (list ex-tuplet-notecontents)))
  (define ex-track-onemeasure-nested (track "out_nested_track" 140 (list ex-tuplet-nested)))
  (define ex-track-twomeasures-simple (track "out_long_simple_track" 122.5 (list ex-tuplet-notecontents ex-tuplet-notecontents)))
  (define ex-track-twomeasures-nested (track "out_long_nested_track" 80 (list ex-tuplet-nested ex-tuplet-nested)))
  (define ex-track-integration ((lambda ()
                                  (define k (load "../examples/samples/k.wav"))
                                  (define s (load "../examples/samples/s.wav"))
                                  (define r (load "../examples/samples/r.wav"))
                                  (define h (load "../examples/samples/h.wav" #:chop? #f))
                                  (define _ (tuplet 1 (list)))
                                  (define three-two (polyrhythm (list (pattern (list k k)) (pattern (list s s s)))))
                                  (track "out_rt_integration_1" 180 (list
                                                              (tuplet 4 (list k r s r))
                                                              (tuplet 4 (list three-two three-two))
                                                              (tuplet 4 (list k (pattern (list r (tuplet 2 (list s _ r _))))))
                                                              (tuplet 4 (list s s s s s s s s))
                                                              (tuplet 4 (list (tuplet 1 (list k k)) (tuplet 1 (list s (tuplet 1 (list s k)))) (tuplet 1 (list r r k k)) (tuplet 1 (list s (tuplet 1 (list s h)))))))))))

  ; assembly examples
  (define ex-track-onemeasure-simple-asm (track-assembly "out_simple_track" (list
                                                            (note-assembly ex-rsound-1 0 #t 52920) (note-assembly ex-rsound-2 52920 #f 105840))))
  (define ex-track-onemeasure-nested-asm (track-assembly "out_nested_track" (list
                                                                         (note-assembly ex-rsound-1 0 #t 1323000/209) (note-assembly ex-rsound-2 1323000/209 #f 2646000/209)
                                                                         (note-assembly ex-rsound-1 661500/19 #t 926100/19) (note-assembly ex-rsound-2 926100/19 #f 1190700/19)
                                                                         (note-assembly ex-rsound-1 2116800/19 #t 2315250/19) (note-assembly ex-rsound-2 2315250/19 #f 132300))))
  (define ex-track-twomeasures-simple-asm (track-assembly "out_long_simple_track" (list
                                                                               (note-assembly ex-rsound-1 0 #t 43200) (note-assembly ex-rsound-2 43200 #f 86400)
                                                                               (note-assembly ex-rsound-1 86400 #t 129600) (note-assembly ex-rsound-2 129600 #f 172800))))
  (define ex-track-twomeasures-nested-asm (track-assembly "out_long_nested_track" (list (note-assembly ex-rsound-1 0 #t 2315250/209) (note-assembly ex-rsound-2 2315250/209 #f 4630500/209)
                                                                                    (note-assembly ex-rsound-1 1157625/19 #t 1620675/19) (note-assembly ex-rsound-2 1620675/19 #f 2083725/19)
                                                                                    (note-assembly ex-rsound-1 3704400/19 #t 8103375/38) (note-assembly ex-rsound-2 8103375/38 #f 231525)
                                                                                    (note-assembly ex-rsound-1 231525 #t 50703975/209) (note-assembly ex-rsound-2 50703975/209 #f 53019225/209)
                                                                                    (note-assembly ex-rsound-1 5556600/19 #t 6019650/19) (note-assembly ex-rsound-2 6019650/19 #f 6482700/19)
                                                                                    (note-assembly ex-rsound-1 8103375/19 #t 16901325/38) (note-assembly ex-rsound-2 16901325/38 #f 463050))))

  ; output examples
  (define ex-track-onemeasure-simple-out (rs-read (test-file-path "simple_track.wav")))
  (define ex-track-onemeasure-nested-out (rs-read (test-file-path "nested_track.wav")))
  (define ex-track-twomeasures-simple-out (rs-read (test-file-path "long_simple_track.wav")))
  (define ex-track-twomeasures-nested-out (rs-read (test-file-path "long_nested_track.wav")))
  (define ex-track-integration-out (rs-read (test-file-path "rt_integration_1.wav")))

  ; note(?) tests
  (check-equal? (note? ex-note-simple) #t)
  (check-equal? (note? "note") #f)
  (check-equal? (note? #t) #f)
  (check-equal? (note? ex-tuplet-simple) #f)
  (check-contract-violation (note "sdf" #f))
  (check-contract-violation (note ex-rsound-1 7))
  (check-contract-violation (note ex-rsound-1 "#t"))

  ; tuplet(?) tests
  (check-equal? (tuplet? ex-tuplet-simple) #t)
  (check-equal? (tuplet? ex-tuplet-nested) #t)
  (check-equal? (tuplet? ex-tuplet-ratbeats) #t)
  (check-equal? (tuplet? ex-pattern-nested) #f)
  (check-contract-violation (tuplet "7" '()))
  (check-contract-violation (tuplet 7 '("not oneshot")))
  (check-contract-violation (tuplet 7 "not list"))
  (check-contract-violation (tuplet -4 '()))
  (check-contract-violation (tuplet 0 '()))
  (check-contract-violation (tuplet 1+2i '()))
  (check-contract-violation (tuplet +inf.0 '()))

  ; pattern(?) tests
  (check-equal? (pattern? ex-pattern-simple) #t)
  (check-equal? (pattern? ex-pattern-nested) #t)
  (check-equal? (pattern? ex-polyrhythm-simple) #f)
  (check-contract-violation (pattern '("not oneshot")))
  (check-contract-violation (pattern "not oneshot"))

  ; polyrhythm(?) tests
  (check-equal? (polyrhythm? ex-polyrhythm-simple) #t)
  (check-equal? (polyrhythm? ex-polyrhythm-desync) #t)
  (check-equal? (polyrhythm? ex-polyrhythm-nested) #t)
  (check-equal? (polyrhythm? ex-pattern-simple) #f)
  (check-contract-violation (polyrhythm '("not oneshot")))
  (check-contract-violation (polyrhythm "not oneshot"))
  (check-contract-violation (polyrhythm '(ex-note-simple)))
  (check-contract-violation (polyrhythm '(ex-tuplet-simple)))

  ; oneshot? tests
  (check-equal? (oneshot? ex-note-simple) #t)
  (check-equal? (oneshot? ex-tuplet-simple) #t)
  (check-equal? (oneshot? ex-pattern-simple) #t)
  (check-equal? (oneshot? ex-polyrhythm-simple) #t)
  (check-equal? (oneshot? #t) #f)
  (check-equal? (oneshot? "amoog") #f)
  (check-equal? (oneshot? 220) #f)

  ; track(?) tests
  (check-equal? (track? ex-track-twomeasures-nested) #t)
  (check-equal? (track? ex-tuplet-simple) #f)
  (check-contract-violation (track "empty_track" 99 (list)))
  (check-contract-violation (track 150 150 (list ex-tuplet-simple)))
  (check-contract-violation (track "invalid_bpm" -150 (list ex-tuplet-simple)))
  (check-contract-violation (track "invalid_bpm" 0 (list ex-tuplet-simple)))
  (check-contract-violation (track "invalid_bpm" 3+5i (list ex-tuplet-simple)))
  (check-contract-violation (track "invalid_bpm" +inf.0 (list ex-tuplet-simple)))
  (check-contract-violation (track "invalid_list" 10 ":3"))
  (check-contract-violation (track "invalid_list" 150 (list ex-tuplet-simple ex-pattern-simple)))

  ; note-assembly(?) tests
  (check-equal? (note-assembly? (note-assembly ex-rsound-1 0 #t 10)) #t)
  (check-equal? (note-assembly? (note-assembly ex-rsound-1 100000 #f 1000000)) #t)
  (check-equal? (note-assembly? (note-assembly ex-rsound-1 4/3 #t 5/3)) #t)
  (check-equal? (note-assembly? ex-note-simple) #f)
  (check-contract-violation (note-assembly ex-rsound-1 -1 #t 10))
  (check-contract-violation (note-assembly ex-rsound-1 3+5i #t 10))
  (check-contract-violation (note-assembly ex-rsound-1 +inf.0 #t 10))
  (check-contract-violation (note-assembly ex-rsound-1 10 10 10))

  ; track-assembly(?) tests
  (check-equal? (track-assembly? ex-track-twomeasures-nested-asm) #t)
  (check-equal? (track-assembly? ex-track-twomeasures-nested) #f)
  (check-contract-violation (track-assembly 99 (list (note-assembly ex-rsound-1 0 #t 1))))
  (check-contract-violation (track-assembly "invalid_list" "stuff"))
  (check-contract-violation (track-assembly "invalid_list" (list)))
  (check-contract-violation (track-assembly "invalid_list" (list ex-note-simple)))

  ; bpm-to-samples tests
  (check-equal? (bpm-to-samples 10) 264600)
  (check-equal? (bpm-to-samples 140) 18900)
  (check-equal? (bpm-to-samples 122.5) 21600)
  (check-equal? (bpm-to-samples pi) 842248)
  (check-contract-violation (bpm-to-samples 0))
  (check-contract-violation (bpm-to-samples -10))
  (check-contract-violation (bpm-to-samples 3+5i))
  (check-contract-violation (bpm-to-samples +inf.0))

  ; squeeze tests
  (check-equal? (squeeze-track ex-track-onemeasure-simple) ex-track-onemeasure-simple-asm)
  (check-equal? (squeeze-track ex-track-onemeasure-nested) ex-track-onemeasure-nested-asm)
  (check-equal? (squeeze-track ex-track-twomeasures-simple) ex-track-twomeasures-simple-asm)
  (check-equal? (squeeze-track ex-track-twomeasures-nested) ex-track-twomeasures-nested-asm)
  (check-contract-violation (squeeze-track ex-tuplet-simple))
  (check-contract-violation (squeeze-oneshot-list ex-tuplet-simple 12 12))
  (check-contract-violation (squeeze-oneshot-list (list ex-track-onemeasure-simple) 12 12))
  (check-contract-violation (squeeze-oneshot-list (list) -12 12))
  (check-contract-violation (squeeze-oneshot-list (list) 12 -12))
  (check-contract-violation (squeeze-tuplet ex-polyrhythm-simple 12 12))
  (check-contract-violation (squeeze-pattern ex-tuplet-simple 12 12))
  (check-contract-violation (squeeze-polyrhythm ex-pattern-simple 12 12))
  (check-contract-violation (squeeze-tuplet ex-tuplet-simple -12 12))
  (check-contract-violation (squeeze-pattern ex-pattern-simple -12 12))
  (check-contract-violation (squeeze-polyrhythm ex-polyrhythm-simple -12 12))
  (check-contract-violation (squeeze-tuplet ex-tuplet-simple 12 -12))
  (check-contract-violation (squeeze-pattern ex-pattern-simple 12 -12))
  (check-contract-violation (squeeze-polyrhythm ex-polyrhythm-simple 12 -12))

  ; assemble-track tests
  (check-equal? (assemble-track ex-track-onemeasure-simple-asm) ex-track-onemeasure-simple-out)
  (check-equal? (assemble-track ex-track-onemeasure-nested-asm) ex-track-onemeasure-nested-out)
  (check-equal? (assemble-track ex-track-twomeasures-simple-asm) ex-track-twomeasures-simple-out)
  (check-equal? (assemble-track ex-track-twomeasures-nested-asm) ex-track-twomeasures-nested-out)
  (check-contract-violation (assemble-track ex-track-onemeasure-simple))

  ; load tests
  (check-equal? (note-sound (load (test-file-path "440hz.wav"))) (note-sound ex-note-simple))
  (check-equal? (note-chop? (load (test-file-path "440hz.wav"))) (note-chop? ex-note-simple))
  (check-equal? (note-chop? (load (test-file-path "880hz.wav") #:chop? #f)) #f)
  (check-equal? (note-sound (load (test-file-path "440hz.wav") #:in 10)) (clip ex-rsound-1 10 1000))
  (check-equal? (note-sound (load (test-file-path "440hz.wav") #:out 500)) (clip ex-rsound-1 0 500))
  (check-equal? (note-sound (load (test-file-path "440hz.wav") #:in 158 #:out 220)) (clip ex-rsound-1 158 220))
  (check-contract-violation (load "." #:in -10))
  (check-contract-violation (load "." #:in 10.5))
  (check-contract-violation (load "." #:out -10))
  (check-contract-violation (load "." #:out 10.5))
  (check-contract-violation (load "." #:chop? -10))

  ; pitch tests
  (check-equal? (note-sound (pitch ex-note-simple 2)) (note-sound (load (test-file-path "440hz-pitch-up-2s.wav"))))
  (check-equal? (note-sound (pitch ex-note-simple 2 50)) (note-sound (load (test-file-path "440hz-pitch-up-2s50c.wav"))))
  (check-equal? (note-sound (pitch ex-note-simple -7)) (note-sound (load (test-file-path "440hz-pitch-down-7s.wav"))))
  (check-equal? (note-sound (pitch ex-note-simple 0)) (note-sound (load (test-file-path "440hz-pitch-0.wav"))))
  (check-contract-violation (pitch ex-tuplet-simple 1))

  ; stretch tests
  (check-equal? (note-sound (stretch ex-note-simple 0.5)) (note-sound (load (test-file-path "440hz-stretch-0.5.wav"))))
  (check-equal? (note-sound (stretch ex-note-simple 2)) (note-sound (load (test-file-path "440hz-stretch-2.wav"))))
  (check-equal? (note-sound (stretch ex-note-simple 1)) (note-sound (load (test-file-path "440hz-stretch-1.wav"))))
  (check-contract-violation (stretch ex-tuplet-simple 1))
  (check-contract-violation (stretch ex-note-simple 0))
  (check-contract-violation (stretch ex-note-simple -1))

  ; resample tests
  (check-equal? (note-sound (resample ex-note-simple 0.5)) (note-sound (load (test-file-path "440hz-resample-0.5.wav"))))
  (check-equal? (note-sound (resample ex-note-simple 2)) (note-sound (load (test-file-path "440hz-resample-2.wav"))))
  (check-equal? (note-sound (resample ex-note-simple 1)) (note-sound (load (test-file-path "440hz-resample-1.wav"))))
  (check-contract-violation (resample ex-tuplet-simple 1))
  (check-contract-violation (resample ex-note-simple 0))
  (check-contract-violation (resample ex-note-simple -1))

  ; note-reverse tests
  (define nleft (load (test-file-path "noise_left.wav")))
  (define nright (load (test-file-path "noise_right.wav")))
  (check-equal? (note-sound (note-reverse nleft)) (note-sound (load (test-file-path "noise_left-rev.wav"))))
  (check-equal? (note-sound (note-reverse nright)) (note-sound (load (test-file-path "noise_right-rev.wav"))))
  (check-contract-violation (note-reverse ex-tuplet-simple))

  ; play! tests
  (check-contract-violation (play! ex-track-onemeasure-simple))

  ; save! tests
  (save! ex-track-onemeasure-simple-asm (test-file-path))
  (check-equal? (rs-read (test-file-path "out_simple_track.wav")) ex-track-onemeasure-simple-out)
  (save! ex-track-onemeasure-nested-asm (test-file-path "out_nested_track.wav"))
  (check-equal? (rs-read (test-file-path "out_nested_track.wav")) ex-track-onemeasure-nested-out)
  (save! ex-track-twomeasures-simple-asm (test-file-path))
  (check-equal? (rs-read (test-file-path "out_long_simple_track.wav")) ex-track-twomeasures-simple-out)
  (save! ex-track-twomeasures-nested-asm (test-file-path "out_long_nested_track.wav"))
  (check-equal? (rs-read (test-file-path "out_long_nested_track.wav")) ex-track-twomeasures-nested-out)

  ; integration
  (check-equal? (assemble-track (squeeze-track ex-track-integration)) ex-track-integration-out)
)