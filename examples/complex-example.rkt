#lang racket

(require tuplet)

(let (k1 k2 s1 (3 h1 s2 h2 s3 k3 k4) s4 (1 r1 r2) k5 k6 s5 (3 h3 s6 h4 s7 (2 s8)) (2 h5)) (load "./samples/cw_amen02_165.wav"))
(let _ (1))
(let three-two (: (k1 k1) (s1 s1 s1)))
(let s-pitch2 (pitch s1 2))
(let s-pitch4 (pitch s1 4))
(let h-nochop (set-chop h5 #f))

(track complex_example 180
       (4 k1 r1 s1 r1)
       (4 three-two three-two)
       (4 k1 (r1 (2 s1 _ r1 _)))
       (4 s1 s1 s1 s1 s3 s3 (1 s-pitch2 s-pitch2) (1 s-pitch4 s-pitch4))
       (4 (1 k1 k1) (1 s1 (1 s1 k1)) (1 r1 r1 k1 k1) (1 s1 (1 s1 h-nochop))))

(play! complex_example)
