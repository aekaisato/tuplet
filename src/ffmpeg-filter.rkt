#lang racket

(require rsound "rsound-bytes.rkt")

(define PCM "pcm_s16le")

(provide (struct-out ff-filter) (struct-out ff-filter-option) apply-ffmpeg-filters)

;      ;;; ;;                                ;;                       
;     ;;;; ;;  ;;                            ;;                    ;; 
;    ;;;;  ;;  ;;   ;;                       ;;                    ;; 
;    ;;        ;;   ;;                                             ;; 
;    ;;    ;;  ;;  ;;;;;   ;;;;   ;; ;;      ;;   ;;; ;;;  ;;;;;;  ;; 
;    ;;;;  ;;  ;;  ;;;;;  ;;;;;;  ;;;;;      ;;  ;;;;;;;;;  ;;;;;; ;; 
;   ;;;;;  ;;  ;;   ;;   ;;;  ;;  ;;; ;      ;;  ;;;;;;;;;  ;;  ;; ;; 
;   ;;;;;  ;;  ;;   ;;    ;;  ;;  ;;         ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;    ;;  ;;   ;;    ;;;;;;  ;;         ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;    ;;  ;;   ;;    ;;      ;;         ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;    ;;  ;;   ;;    ;;  ;;  ;;         ;;  ;; ;;; ;;  ;;  ;; ;; 
;    ;;    ;;  ;;   ;;    ;;  ;;  ;;         ;;  ;; ;;; ;;  ;;;;;; ;; 
;    ;;    ;;  ;;   ;;;;  ;;;;;   ;;         ;;  ;; ;;; ;;  ;;;;;  ;; 
;    ;;    ;;  ;;     ;;   ;;;    ;;         ;;  ;;     ;;  ;;     ;; 
;                                                           ;;        
;                                                           ;;        
;                                                           ;;        

; a filter-option is a key and value string, representing an option on an ffmpeg filter
(define-struct/contract ff-filter-option ([key string?] [value string?]))

; a filter is a filter name and a list of options, which may be either key-value pairs or a single string
(define-struct/contract ff-filter ([name string?] [opts (listof (or/c ff-filter-option? string?))]))

(define ffmpeg-string-template "ffmpeg -ar ~a -f s16le -i - -filter:a \"~a\" -c:a pcm_s16le -f s16le -")
(define ffmpeg-ver-cmd "ffmpeg -version")
(define enable-librubberband-str "--enable-librubberband")

; 0 for unchecked, positive for compatible, negative for incompatible
(define ffmpeg-is-compatible (box 0))

; applies the given ffmpeg filters to the given rsound by running ffmpeg as a subprocess
(define/contract (apply-ffmpeg-filters rs filters)
  (-> rsound? (listof ff-filter?) rsound?)
  (when (equal? (unbox ffmpeg-is-compatible) 0)
    (define verstr (get-ffmpeg-versionstring))
    (set-box! ffmpeg-is-compatible (if (string-contains? verstr enable-librubberband-str) 1 0)))
  (when (< (unbox ffmpeg-is-compatible) 0)
    (error "compatible ffmpeg not found - ensure ffmpeg is installed, on the PATH, and compiled with librubberband"))
  (match-let ([(s16le bstr sample-rate) (rsound->s16le rs)])
    (define cmdstr (create-ffmpeg-cmdstr filters sample-rate))
    (define ffmpeg-bout (run-cmd-with-bytes cmdstr bstr))
    (define res (s16le->rsound (s16le ffmpeg-bout sample-rate)))
    res))

; attempts to run ffmpeg, returning the full output of getting its version
(define/contract (get-ffmpeg-versionstring)
  (-> string?)
  (with-output-to-string
    (lambda () (system ffmpeg-ver-cmd))))

; uses the given filters and sample rate to construct an ffmpeg command to pipe s16le bytes to/from
(define/contract (create-ffmpeg-cmdstr filters sample-rate)
  (-> (listof ff-filter?) (and/c integer? positive?) string?)
  (define exact-sr (inexact->exact sample-rate))
  (format ffmpeg-string-template sample-rate (construct-filter-string filters)))

; runs given command string, piping the in bytes to stdin, and returning stdout as a bytestring
(define/contract (run-cmd-with-bytes cmdstr in)
  (-> string? bytes? bytes?)
  (with-output-to-bytes
    (lambda ()
      (with-input-from-bytes in
        (lambda ()
          (parameterize ([current-error-port (open-output-string "")]) (system cmdstr)))))))

; builds a string to include in an ffmpeg command, representing the filters provided
(define/contract (construct-filter-string lof)
  (-> (listof ff-filter?) string?)
  (string-join (map ff-filter->string lof) ", "))

; converts a single filter into a string representing it and its options
(define/contract (ff-filter->string f)
  (-> ff-filter? string?)
  (format "~a=~a" (ff-filter-name f) (opts->string (ff-filter-opts f))))

; converts a list of filter options into a single string representing each option
(define/contract (opts->string l)
  (-> (listof (or/c ff-filter-option? string?)) string?)
  (string-join (map (lambda (o)
                      (match o
                        [(ff-filter-option k v) (format "~a=~a" (ff-filter-option-key o) (ff-filter-option-value o))]
                        [(? string? s) s]))
                    l) ":"))