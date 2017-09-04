#lang racket

(define (interp code input)
  (struct pos (pre cur suf) #:transparent)
  (define (shift-left p)
    (if (null? (pos-pre p))
        (pos null
             0
             (cons (pos-cur p)
                   (pos-suf p)))
        (pos (cdr (pos-pre p))
             (car (pos-pre p))
             (cons (pos-cur p)
                   (pos-suf p)))))
  (define (shift-right p)
    (if (null? (pos-suf p))
        (pos (cons (pos-cur p)
                   (pos-pre p))
             0
             null)
        (pos (cons (pos-cur p)
                   (pos-pre p))
             (car (pos-suf p))
             (cdr (pos-suf p)))))
  (define (add-current p)
    (pos (pos-pre p)
         (add1 (pos-cur p))
         (pos-suf p)))
  (define (sub-current p)
    (pos (pos-pre p)
         (sub1 (pos-cur p))
         (pos-suf p)))
  (define (set-current p c)
    (pos (pos-pre p)
         (char->integer c)
         (pos-suf p)))
  (define (current-val p)
    (integer->char (pos-cur p)))
  
  (define (interp-list loc input p output)
    (cond [(null? loc) output]
          [(eq? (car loc) #\>)
           (interp-list (cdr loc) input (shift-right p) output)]
          [(eq? (car loc) #\<)
           (interp-list (cdr loc) input (shift-left p) output)]
          [(eq? (car loc) #\+)
           (interp-list (cdr loc) input (add-current p) output)]
          [(eq? (car loc) #\-)
           (interp-list (cdr loc) input (sub-current p) output)]
          [(eq? (car loc) #\.)
           (interp-list (cdr loc) input p (string-append output (string (current-val p))))]
          [(eq? (car loc) #\,)
           (interp-list (cdr loc) (cdr input) (set-current p (car input)) output)]
          [else
           (error "interp: unexpected character found" (car loc))]))
  (interp-list (string->list code) (string->list input) (pos null 0 null) ""))
