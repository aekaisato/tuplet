#lang racket

(require tuplet)

(let k (load "./samples/k.wav"))
(let s (load "./samples/s.wav"))
(let r (load "./samples/r.wav"))
(let h (load "./samples/h.wav" #:chop? #f))
(let _ (1))
(let three-two (: (k k) (s s s)))

(track basic_example 180
       (4 k r s r)
       (4 three-two three-two)
       (4 k (r (2 s _ r _)))
       (4 s s s s s s s s)
       (4 (1 k k) (1 s (1 s k)) (1 r r k k) (1 s (1 s h))))

(play! basic_example)
