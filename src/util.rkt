#lang racket

(module+ test
  (require rackunit (for-syntax syntax/parse) syntax/macro-testing)
  (provide (all-defined-out))

  ; macro around check-exn that inserts the lambda for the user
  (define-syntax check-except
    (lambda (stx)
      (syntax-parse stx
        [(_ expr subst) (quasisyntax #,(syntax/loc stx (check-exn (regexp subst) (lambda () expr))))])))

  (define-syntax check-syntax-except
    (lambda (stx)
      (syntax-parse stx
        [(_ expr subst) (quasisyntax #,(syntax/loc stx (check-exn (regexp subst) (lambda () (convert-syntax-error (lambda () expr))))))])))

  ; allows user to check for a contract violation, without typing in a lambda
  (define-syntax check-contract-violation
    (lambda (stx)
      (syntax-parse stx
        [(_ expr) (quasisyntax #,(syntax/loc stx (check-exn #rx"contract violation" (lambda () expr))))])))

  ; used to more easily access commonly used directories
  ; all of these functions assume cwd is one subdir below root
  (define (test-file-path [str ""]) (string-append "../examples/for_testing/" str))
  (define (sample-path [str ""]) (string-append "../examples/samples/" str))
  )