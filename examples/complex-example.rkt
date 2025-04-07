#lang racket

(require tuplet)

(let (k1 k2 s1 (3 h1 s2 h2 s3 k3 k4) s4 (1 r1 r2) k5 k6 s5 (3 h3 s6 h4 s7 (2 s8)) (2 h5)) (load "./samples/cw_amen02_165.wav"))
(let _ (1))
(let s-pitch2 (pitch s1 2))
(let s-pitch4 (pitch s1 4))
(let k-down (pitch k2 -2))
(let s-rev (reverse s1))

(track complex_example 180
       (4 (1 k1 k1) (1 s1 (1 s1 k1)) (1 r1 r2 k2 k1) (1 s1 (1 s3 s1)))
       (4 (1 k1 k1) (1 s1 (1 k1 k1)) (2 s1 k2 k2 s1 k2 k2 (2 h5)))
       (4 (1 k1 k1 k1) (1 s1 h5) (1 k2 k2 k2) (1 s1 (1 s-pitch2 s-pitch4)))
       (4 (1 k1 k1) (1 s1 (1 s1 k1)) (1 k1 k1 k2 k2) (1 s1 s1 s1 s1 s1 s1 s1))

       (4 (1 s-pitch2 (1 k1 k1)) (1 s3 s3 s3) (1 h5 (1 k1 k1)) (1 s1 s2 s1))
       (4 (1 (s1 s3 s1 s3 s1 s3 s1 s3)) (1 h5 h5) (1 k-down (1 k-down k-down)) (1 s1 s-rev))
       (4 (1 s-pitch2 (1 k1 k1)) (1 s1 s1) (1 (1 k1 k1) s1) (1 s1 (1 s-pitch2 s-pitch2 s-pitch4)))
       (4 (1 k1 s-rev) (1 k1 s-rev) (1 (: (k1 k1) (s1 s1 s1))) (1 k1 (1 s-pitch2 s-pitch2 s-pitch2 s-pitch2 s-pitch2)))
       )

(play! complex_example)
