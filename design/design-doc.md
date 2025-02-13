# Tuplet

an esoteric, programmatic music notation geared towards breakcore production

## Background

## Domain Concepts

## Linguistic Features

## Examples

### Simple Rhythm

```
(let o (load-file “kick.wav”))
(let x (load-file “snare.wav”))
(let pitched (pitch x 5)) ; pitched up 5 semitones
(let pattern (o x o x))
(let triplet (1 o o o))
(let rest (1))
(track “track_1” 150 (
  (4 o x o x) ; these two measures
  (4 pattern) ; are equivalent
  (4 o x (1 o o) (1 rest pitched))
  (4 o x triplet x)
))
```

This is a very basic example of a track defined using Tuplet's proposed syntax.
Each measure is in 4/4, with a simple beat consisting of loaded kick and snare
samples. Tuplets are used both inlined in the track and bound to a name to
create eighth notes and eighth note triplets. A rest is created by defining
an empty tuplet. The `pitch` function is used to defined a pitched up copy
of the snare drum sample.

This is what an equivalent beat would look like when written in music notation
and FL Studio, respectively:

![a simple beat of quarter notes and eighth notes, notated in musescore](./assets/simple-ex-musicnotation.png)
![a simple beat of quarter notes and eighth notes, notated in fl studio](./assets/simple-ex-flstudio.png)

### "Polyriddim"

```
(let w (load-file “wub.wav”))
(let _ (1))
(track “polyriddim_drop_wubs” 122.5 (
  (7 (3 _ w w w w w w) (4 _ w w w w w w w w w w))
  (7 (14 (3 _ w w w) w (2 _ w w) (8 (3 _ w w w) (3 w w w w) w w w w w)))
  (7 (3 _ w (3 w w w w) (2 w w w w)) (1 _ w w) (1 w w w w) (1 w w w w w) (1 w w w w w))
  (7 (1 _ w w) (1 w w w) (1 w w w) (4 _ w w w w (6 (2 w) w w w w w w w w w)))
  (7 _ _ (1 _ w w w w) (1 _ w w w w) (1 w w w w w) (1 w (1 w w)) (1 w w w w))
  (7 (2 _ w w w w (1/2 w)) (1 _ w w w) (1 _ w w w) (1 w w w w) (1 w w w w) (1 w w w w))
  (7 _ _ (1 _ w w w w) (1 _ w w w w) (1 w w w w w w) (1 w (1 w w)) (1 w w w w w))
  (7 (1 w w w w) (1 w w w w) (5 (1 w w w) (1 w w) (3 w w w w w w w w)))
))
```

The song "polyriddim" by phonon is a dubstep track with idiosyncratic rhythms during the
drops, most notably featuring heavy use of nested tuplets. (A live drummed
example of this, alongside music notation, can be watched
[here](https://www.youtube.com/watch?v=xv05y31U1p4&t=827)). In this example,
the first drop of "polyriddim" is used as a stress test for the expressivity
and flexibility of Tuplet's rhythmic notation. Nested tuplets are, in fact,
a very natural part of the language, allowing for some very complex
rhythms to be rapidly written. Because tuplets fit the contents to the number
of beats specified, something like `(7 (14 ...))` allows the user to work
in terms of eighth notes, instead of quarter notes.

### Breakcore

```
(let (k1 k2 s1 (3 h1 s2 h2 s3 k3 k4) s4 (1 r1 r2) k5 k6 s5 (3 h3 s6 h4 s7 (2 s8)) (2 h5)) (load-file-split “cw_amen02_165.wav”))
(let _ (1)) (let two_snare (1 s1 s1)) (let two_kick (1 k1 k2))
(let ss_s (1 two_snare s1) (let s_ss (1 s1 two_snare)) (let kk_s (1 two_kick s1) (let s_kk (1 s1 two_kick))
(let h5_pitched (pitch h5 2)) (let s1_pitched (pitch s1 2)) (let k2_pitched (pitch k2 -2)) (let s1_pitched_2 (pitch s1 4))
(let pattern1 (two_kick s_ss (1 h2 h1 k3 k3)))
(track “breakcore_beat_part1” 180 (
  (4 pattern1 (1 s1 (1 s3 r1)))
  (4 (1 k1 (1 s1 s1 s1 s1 s1 _)) s_kk (1 h4 _ h3 _) (h5 h5_pitched))
  (4 (1 k1 (1 k1 k1)) (1 s1 k3 h1 k3) (1 s3 _ h2 k1) (1 s1 s1_pitched))
  (4 (1 k1 k1) (1 ss_s (1 k1 _)) (1 s1 (1 k1 _)) (1 s1 ss_s))
  (4 pattern1 (1 (k1 (1 s1 _)) s1))
  (4 (1 h1 _ (2 s3)) (1 k2 k2 k2 _) (1 h5 (1 _ (2 h2) _)) h5)
  (4 k1 k2 s1 (1 h1 s2))
  (4 (2 (2 h5) s1 s1 s1 h1 (2 k1)) (1 (1 s1 s1 s1 s1) k2) (1 (2 s1 s1 s1 s1 s1 s1 s1 _) s1_pitched_2 s1_pitched))
))
```

Finally moving to an example within the intended domain, this is eight measures
of the drum splicing of a breakcore excerpt. The actual song can be listened to
[here](https://youtu.be/afmh_KJUBbI?t=21). The first aspect of note is how a
pattern is used to bind segments of an amen break into names. Otherwise, the
rest of the example is fairly straightforward, using the features covered
in the previous examples. While it may still be somewhat complicated to
glance at, the rate at which changes can be made and tested is much
higher than other ways one would write the same beat.

Once Tuplet is implemented, the exported audio would be equivalent to the
manual splicing seen here:

![a screenshot of a large number of individually sliced audio clips in fl studio](./assets/breakcore-ex-flstudio.png)

## Syntax

## Implementation Milestones

The following is a rough outline and explanation of the steps we will
take to implement this language.

### Define data representations for oneshots and tuplets

The majority of our language's functionality is encoded by the data
that the user defines. The reason why patterns aren't included is
because they would likely directly be implemented via macros. We
may also build a intermediate "flattened" data representation
generalized for track exports.

### Implement runtime functions for loading and exporting oneshots

Before implementing the core functionality, we'd like to ensure
the audio pipeline works as intended, simply from load to export.

### Implement runtime functions for “squeezing” and exporting tuplets

The core functionality is implemented in this step. We would likely
process the track to a "flattened" track that is then passed to functions
for audio or MIDI export.

### Implement macros for tuplets

These macros allow us to get closer to our intended surface syntax. We
decided to do this first because it is the most integral feature to
get right, syntax-wise.

### Implement macros for variable binding

Adding basic variable binding will greatly improve the ergonomics of our language.
Binding based on a pattern structure will be added much later.

### Implement macros for patterns

Once we add syntax for patterns (which only exist in syntax), we can build
tracks identical to the examples (though the variable definitions will have
to be different to compensate for a lack of features).

### Add utility functions for transforming oneshots (e.g. reverse, pitch shift)

These functions would improve the ergonomics of loading and using files,
since you don't have to bounce to a different piece of software to accompolish
common edits to an audio sample.

### Add runtime functions and macro for splicing samples during load

This is an arguably less crucial, yet still important feature for our language.
It builds off of previous work on patterns, loading files, and tuplet squeezing.

### Add MIDI export

MIDI export isn't super necessary, but it's a nice-to-have that shouldn't be
difficult to add, assuming we did our previous steps sufficiently.
