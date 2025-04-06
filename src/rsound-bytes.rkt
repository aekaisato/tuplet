#lang racket

(require ffi/vector rsound racket/block)
(provide rsound->s16le s16le->rsound (struct-out s16le))

;                   ;;;                     ;;                       
;           ;;;    ;;;;;   ;;               ;;                    ;; 
;           ;;;   ;;;;;;;  ;;               ;;                    ;; 
;            ;;   ;;   ;;  ;;                                     ;; 
;    ;;;;    ;;   ;;       ;;   ;;;;        ;;   ;;; ;;;  ;;;;;;  ;; 
;   ;;;;;;   ;;   ;;       ;;  ;;;;;;       ;;  ;;;;;;;;;  ;;;;;; ;; 
;   ;;  ;;   ;;   ;;;;;;   ;; ;;;  ;;       ;;  ;;;;;;;;;  ;;  ;; ;; 
;   ;;       ;;   ;;;;;;;  ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;;;     ;;   ;;   ;;  ;;  ;;;;;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;;;;   ;;   ;;   ;;  ;;  ;;           ;;  ;; ;;; ;;  ;;  ;; ;; 
;       ;;   ;;   ;;   ;;  ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;  ;; ;; 
;   ;;  ;;   ;;   ;;   ;;  ;;  ;;  ;;       ;;  ;; ;;; ;;  ;;;;;; ;; 
;   ;;;;;    ;;    ;;;;;;  ;;  ;;;;;        ;;  ;; ;;; ;;  ;;;;;  ;; 
;     ;;     ;;     ;;;    ;;   ;;;         ;;  ;;     ;;  ;;     ;; 
;                                                          ;;        
;                                                          ;;        
;                                                          ;;        

; a s16le represents the raw data of a sound and the sample rate needed to handle it
(define-struct/contract s16le ([bstr bytes?] [sample-rate (and/c integer? positive?)]))

; constants from rsound https://github.com/jbclements/RSound/blob/ec9bbcf9120aa4678b36126fba119ae8433d78c3/rsound/read-wav.rkt#L34
(define global-bitspersample 16)
(define global-bytespersample (* global-bitspersample 1/8))
(define global-numchannels 2)
(define global-samplemax (exact->inexact #x8000))

; converts a s16le to an equivalent rsound by converting the bytes to a s16vector
; based on rsound's read-wav.rkt (https://github.com/jbclements/RSound/blob/ec9bbcf9120aa4678b36126fba119ae8433d78c3/rsound/read-wav.rkt)
(define/contract (s16le->rsound data)
  (-> s16le? rsound?)
  (match data
    [(s16le bstr sample-rate)
     (define frames-to-read (/ (bytes-length bstr) (* global-numchannels global-bytespersample)))
     (define vec (make-s16vector (* global-numchannels frames-to-read)))
     (for ([j (in-range (* 2 frames-to-read))])
       (define i (* global-bytespersample j))
       (s16vector-set! vec j (integer-bytes->integer (subbytes bstr i (+ i 2)) #t #f)))
     (vec->rsound vec sample-rate)]))

; converts an rsound to an equivalent s16le by converting the internal s16vector to bytes
; based on rsound's write-wav.rkt (https://github.com/jbclements/RSound/blob/ec9bbcf9120aa4678b36126fba119ae8433d78c3/rsound/write-wav.rkt)
(define/contract (rsound->s16le rs)
  (-> rsound? s16le?)
  (match rs
    [(rsound data start stop sample-rate)
     (s16le
      (with-output-to-bytes
        (lambda ()
          (define port (current-output-port))
          (for ([i (in-range start stop)])
            (define sample (* i global-numchannels))
            (display (integer->integer-bytes (s16vector-ref data sample) 2 #t #f) port)
            (display (integer->integer-bytes (s16vector-ref data (add1 sample)) 2 #t #f) port)))) sample-rate)]))