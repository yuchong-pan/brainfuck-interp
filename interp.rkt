#lang racket

(define (interp code input)
  (struct pos (pre cur suf) #:transparent)
  
  (define (shift-left p)
    (if (null? (pos-pre p))
        (pos null 0 (cons (pos-cur p) (pos-suf p)))
        (pos (cdr (pos-pre p))
             (car (pos-pre p))
             (cons (pos-cur p) (pos-suf p)))))
  
  (define (shift-right p)
    (if (null? (pos-suf p))
        (pos (cons (pos-cur p) (pos-pre p)) 0 null)
        (pos (cons (pos-cur p) (pos-pre p))
             (car (pos-suf p))
             (cdr (pos-suf p)))))
  
  (define (add-current p)
    (pos (pos-pre p) (add1 (pos-cur p)) (pos-suf p)))
  
  (define (sub-current p)
    (pos (pos-pre p) (sub1 (pos-cur p)) (pos-suf p)))
  
  (define (set-current p c)
    (pos (pos-pre p) (char->integer c) (pos-suf p)))
  
  (define (current-val p)
    (pos-cur p))

  (struct state (input p output) #:transparent)

  (define (match-left stack result raise-error)
    (cond [(null? stack)
           (if raise-error
               (error "square brackets not matched")
               #f)]
          [(eq? (car stack) #\[)
           (cons result (cdr stack))]
          [else
           (match-left (cdr stack) (cons (car stack) result) raise-error)]))

  (define (pre-process loc stack)
    (cond [(null? loc)
           (if (match-left stack null #f)
               (error "square brackets not matched")
               (reverse stack))]
          [(eq? (car loc) #\])
           (pre-process (cdr loc) (match-left stack null #t))]
          [else
           (pre-process (cdr loc) (cons (car loc) stack))]))
  
  (define (interp-list loc s)
    (let ([input (state-input s)]
          [p (state-p s)]
          [output (state-output s)])
      (cond [(null? loc) s]
            [(list? (car loc))
             (repeat (car loc) (cdr loc) s)]
            [(eq? (car loc) #\>)
             (interp-list (cdr loc) (state input (shift-right p) output))]
            [(eq? (car loc) #\<)
             (interp-list (cdr loc) (state input (shift-left p) output))]
            [(eq? (car loc) #\+)
             (interp-list (cdr loc) (state input (add-current p) output))]
            [(eq? (car loc) #\-)
             (interp-list (cdr loc) (state input (sub-current p) output))]
            [(eq? (car loc) #\.)
             (interp-list (cdr loc) (state input p (cons (integer->char (current-val p)) output)))]
            [(eq? (car loc) #\,)
             (interp-list (cdr loc) (state (cdr input) (set-current p (car input)) output))]
            [else
             (interp-list (cdr loc) s)])))
  
  (define (repeat body rest s)
    (if (zero? (current-val (state-p s)))
        (interp-list rest s)
        (repeat body rest (interp-list body s))))
  
  (list->string (reverse (state-output (interp-list (pre-process (string->list code) null)
                                                    (state (string->list input) (pos null 0 null) null))))))
