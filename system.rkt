#lang racket
(require csfml)

(provide (all-defined-out))

; ------------------------------------------------------
; Angle.hpp
; ------------------------------------------------------

(define (positive-remainder a b)
  (set! a (exact->inexact a))
  (set! b (exact->inexact b))
  (when (> b 0)
    (let ([val (- a (* (truncate (/ a b)) b))])
      (if (>= val 0)
          val
          (+ val b)))))
         
(define-generics angle-funcs
  (angle-radians angle-funcs))

(struct angle (degrees)
  #:transparent
  #:mutable
  #:methods gen:angle-funcs
  [(define (angle-radians angle) (* (angle-degrees angle) (/ pi 180)))])

(define (degrees num)
  (set! num (exact->inexact num))
  (angle num))

(define (radians num)
  (set! num (exact->inexact num))
  (angle (* num (/ 180 pi))))

(define angle-zero (angle 0.))

; ------------------------------------------------------
; Vector2.hpp
; ------------------------------------------------------

(define (vector2f x y)
  (make-sfVector2f (exact->inexact x)
                   (exact->inexact y)))

(define make-v2f vector2f)

(define (vector2i x y)
  (make-sfVector2i (inexact->exact
                    (truncate x))
                   (inexact->exact
                    (truncate y))))

(define make-v2i vector2i)

(define (vector2u x y)
  (make-sfVector2u (abs
                    (inexact->exact
                     (truncate x)))
                   (abs
                    (inexact->exact
                     (truncate y)))))

(define make-v2u vector2u) 

(define vector2f? sfVector2f?)
(define vector2f-x sfVector2f-x)
(define vector2f-y sfVector2f-y)
(define set-vector2f-x! set-sfVector2f-x!)
(define set-vector2f-y! set-sfVector2f-y!)

(define v2f? sfVector2f?)
(define v2f-x sfVector2f-x)
(define v2f-y sfVector2f-y)
(define set-v2f-x! set-sfVector2f-x!)
(define set-v2f-y! set-sfVector2f-y!)

(define vector2i? sfVector2i?)
(define vector2i-x sfVector2i-x)
(define vector2i-y sfVector2i-y)
(define set-vector2i-x! set-sfVector2i-x!)
(define set-vector2x-y! set-sfVector2i-y!)

(define v2i? sfVector2i?)
(define v2i-x sfVector2i-x)
(define v2i-y sfVector2i-y)
(define set-v2i-x! set-sfVector2i-x!)
(define set-v2i-y! set-sfVector2i-y!)

(define vector2u? sfVector2u?)
(define vector2u-x sfVector2u-x)
(define vector2u-y sfVector2u-y)
(define set-vector2u-x! set-sfVector2u-x!)
(define set-vector2u-y! set-sfVector2u-y!)

(define v2u? sfVector2u?)
(define v2u-x sfVector2u-x)
(define v2u-y sfVector2u-y)
(define set-v2u-x! set-sfVector2u-x!)
(define set-v2u-y! set-sfVector2u-y!)

(define-syntax vector2-x
  (syntax-rules ()
    [(_ v2-clause)
     (cond [(vector2f? v2-clause)
            (sfVector2f-x v2-clause)]
           [(vector2i? v2-clause)
            (sfVector2i-x v2-clause)]
           [(vector2u? v2-clause)
            (sfVector2u-x v2-clause)])]))

(define-syntax set-vector2-x!
  (syntax-rules ()
    [(_ a b)
     (cond [(vector2f? a)
            (set-sfVector2f-x! a (exact->inexact b))]
           [(vector2i? a)
            (set-sfVector2i-x! a (inexact->exact (truncate b)))]
           [(vector2u? a)
            (set-sfVector2u-x! a (abs (inexact->exact (truncate b))))])]))

(define (v2x v) (vector2-x v))

(define (set-v2x! a b) (set-vector2-x! a b))

(define-syntax vector2-y
  (syntax-rules ()
    [(_ v2-clause)
     (cond [(vector2f? v2-clause)
            (sfVector2f-y v2-clause)]
           [(vector2i? v2-clause)
            (sfVector2i-y v2-clause)]
           [(vector2u? v2-clause)
            (sfVector2u-y v2-clause)])]))

(define (v2y v) (vector2-y v))

(define-syntax set-vector2-y!
  (syntax-rules ()
    [(_ a b)
     (cond [(vector2f? a)
            (set-sfVector2f-y! a (exact->inexact b))]
           [(vector2i? a)
            (set-sfVector2i-y! a (inexact->exact (truncate b)))]
           [(vector2u? a)
            (set-sfVector2u-y! a (abs (inexact->exact (truncate b))))])]))

(define (set-v2y! a b) (set-vector2-y! a b))

(define-syntax vector2f->vector2i
  (syntax-rules ()
    [(_ v2-clause)
     (cond [(vector2f? v2-clause)
            (vector2i (vector2f-x v2-clause)
                      (vector2f-y v2-clause))]
           [else v2-clause])]))

(define-syntax vector2i->vector2f
  (syntax-rules ()
    [(_ v2-clause)
     (cond [(vector2i? v2-clause)
            (vector2f (vector2i-x v2-clause)
                      (vector2i-y v2-clause))]
           [(vector2u? v2-clause)
            (vector2f (vector2u-x v2-clause)
                      (vector2u-y v2-clause))]
           [else v2-clause])]))

(define-syntax vector2f->vector2u
  (syntax-rules ()
    [(_ v2-clause)
     (cond [(vector2f? v2-clause)
            (vector2u (vector2f-x v2-clause)
                      (vector2f-y v2-clause))]
           [(vector2i? v2-clause)
            (vector2u (vector2i-x v2-clause)
                      (vector2i-y v2-clause))]
           [else v2-clause])]))



(define-syntax add-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2f? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2f (+ (vector2-x out)
                                   (vector2-x (car in)))
                                (+ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2i (+ (vector2-x out)
                                   (vector2-x (car in)))
                                (+ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2u (+ (vector2-x out)
                                   (vector2-x (car in)))
                                (+ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]))]))

(define-syntax sub-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2f? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (when (not (eq? in null))
                  (set! out (lp (cdr in)
                      (vector2f (- (vector2-x out)
                                   (vector2-x (car in)))
                                (- (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2i (- (vector2-x out)
                                   (vector2-x (car in)))
                                (- (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2u (- (vector2-x out)
                                   (vector2-x (car in)))
                                (- (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]))]))

(define-syntax mul-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2f? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2f (* (vector2-x out)
                                   (vector2-x (car in)))
                                (* (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2i (* (vector2-x out)
                                   (vector2-x (car in)))
                                (* (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2u (* (vector2-x out)
                                   (vector2-x (car in)))
                                (* (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]))]))

(define-syntax mod-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2i (modulo (vector2-x out)
                                                  (vector2-x (car in)))
                                          (modulo (vector2-y out)
                                                  (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2u (modulo (vector2-x out)
                                                  (vector2-x (car in)))
                                          (modulo (vector2-y out)
                                                  (vector2-y (car in)))))))
                out)]))]))


(define-syntax quo-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2i (quotient (vector2-x out)
                                                    (vector2-x (car in)))
                                          (quotient (vector2-y out)
                                                    (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2u (quotient (vector2-x out)
                                                    (vector2-x (car in)))
                                          (quotient (vector2-y out)
                                                    (vector2-y (car in)))))))
                out)]))]))

(define-syntax rem-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2i (remainder (vector2-x out)
                                                     (vector2-x (car in)))
                                          (remainder (vector2-y out)
                                                     (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector2u (remainder (vector2-x out)
                                                     (vector2-x (car in)))
                                          (remainder (vector2-y out)
                                                     (vector2-y (car in)))))))
                out)]))]))

(define-syntax div-v2
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (cond [(vector2f? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2f (/ (vector2-x out)
                                   (vector2-x (car in)))
                                (/ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2i? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2i (/ (vector2-x out)
                                   (vector2-x (car in)))
                                (/ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]
             [(vector2u? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                      (vector2u (/ (vector2-x out)
                                   (vector2-x (car in)))
                                (/ (vector2-y out)
                                   (vector2-y (car in)))))))
                out)]))]))

(define (pair->vector2f p)
  (cond [(pair? p)
         (vector2f (car p)
                   (cdr p))]
        [else #f]))

(define (pair->vector2i p)
  (cond [(pair? p)
         (vector2i (car p)
                   (cdr p))]
        [else #f]))

(define (pair->vector2u p)
  (cond [(pair? p)
         (vector2u (car p)
                   (cdr p))]
        [else #f]))

(define (vector2->pair v2)
  (if (or (vector2f? v2)
          (vector2i? v2)
          (vector2u? v2))
      (cons (vector2-x v2)
            (vector2-y v2))
      null))

; ---------------------------------------------------------------------------------
; Vector3.hpp
; ---------------------------------------------------------------------------------

(define (vector3 x y z)
  (make-sfVector3f (exact->inexact x)
                   (exact->inexact y)
                   (exact->inexact z)))

(define (list->vector3 lst)
  (if (and (list? lst)
           (= (length lst) 3))
      (vector3 (car lst)
               (cadr lst)
               (caddr lst))
      #f))

(define vector3? sfVector3f?)
(define vector3-x sfVector3f-x)
(define vector3-y sfVector3f-y)
(define vector3-z sfVector3f-z)

(define set-vector3-x! set-sfVector3f-x!)
(define set-vector3-y! set-sfVector3f-y!)
(define set-vector3-z! set-sfVector3f-z!)

(define (vector3->list v3)
  (if (vector3 v3)
      (list (vector3-x v3)
            (vector3-y v3)
            (vector3-z v3))
      null))

(define v3! vector3)
(define v3? sfVector3f?)
(define v3x sfVector3f-x)
(define v3y sfVector3f-y)
(define v3z sfVector3f-z)
(define set-v3x! set-sfVector3f-x!)
(define set-v3y! set-sfVector3f-y!)
(define set-v3z! set-sfVector3f-z!)

(define-syntax add-v3
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (when (vector3? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector3 (+ (vector3-x out) (vector3-x (car in)))
                                         (+ (vector3-y out) (vector3-y (car in)))
                                         (+ (vector3-z out) (vector3-z (car in)))))))
                out)))]))

(define-syntax sub-v3
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (when (vector3? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector3 (- (vector3-x out) (vector3-x (car in)))
                                         (- (vector3-y out) (vector3-y (car in)))
                                         (- (vector3-z out) (vector3-z (car in)))))))
                out)))]))

(define-syntax mul-v3
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (when (vector3? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector3 (* (vector3-x out) (vector3-x (car in)))
                                         (* (vector3-y out) (vector3-y (car in)))
                                         (* (vector3-z out) (vector3-z (car in)))))))
                out)))]))

(define-syntax div-v3
  (syntax-rules ()
    [(_ v2-clause-a v2-clause-b ...)
     (let ([ls (list v2-clause-b ...)])
       (when (vector3? v2-clause-a)
              (let lp ([in ls]
                       [out v2-clause-a])
                (unless (eq? in null)
                  (set! out (lp (cdr in)
                                (vector3 (/ (vector3-x out) (vector3-x (car in)))
                                         (/ (vector3-y out) (vector3-y (car in)))
                                         (/ (vector3-z out) (vector3-z (car in)))))))
                out)))]))

; -------------------------------------------------------------------------------------------------------
; Time.hpp
; -------------------------------------------------------------------------------------------------------

(define time-zero (make-sfTime 0))
(define time-struct make-sfTime)
(define time-struct? sfTime?)

(define (as-seconds t)
  (if (time-struct? t)
      (sfTime_asSeconds t)
      0))

(define (as-milliseconds t)
  (if (time-struct? t)
      (sfTime_asMilliseconds t)
      0))

(define (as-microseconds t)
  (if (time-struct? t)
      (sfTime_asMicroseconds t)
      0))
  
(define (seconds num)
  (sfSeconds (exact->inexact num)))

(define (milliseconds num)
  (sfMilliseconds (inexact->exact (round num))))

(define (microseconds num)
  (sfMicroseconds (inexact->exact (round num))))

; ---------------------------------------------------------------------------------------------------------
; Clock.hpp
; ---------------------------------------------------------------------------------------------------------

(define clock%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfClock_create))))

    (define/public (kill)
      (when ptr
        (sfClock_destroy ptr)
        (set! ptr #f)))

    (define/public (copy)
      (if ptr
        (sfClock_copy ptr)
        #f))
    
    (define/public (pointer) ptr)
    
    (define/public (set-pointer new-ptr)
      (when (sfClock*? new-ptr)
        (send this kill)
        (set! ptr new-ptr)))
    
    (define/public (get-elapsed-time)
      (if ptr
          (sfClock_getElapsedTime ptr)
          time-zero))

    (define/public (restart)
      (if ptr
          (sfClock_restart ptr)
          time-zero))))

(define (clock? clock-clause)
  (is-a? clock-clause clock%))

; ---------------------------------------------------------------------------------------
; Mutex.hpp
; ---------------------------------------------------------------------------------------

(define mutex%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfMutex_create))))

    (define/public (kill)
      (when ptr
        (sfMutex_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfMutex*? p)
        (send this kill)
        (set! ptr p)))
    
    (define/public (lock)
      (when ptr
        (sfMutex_lock ptr)))

    (define/public (unlock)
      (when ptr
        (sfMutex_unlock ptr)))))

(define (mutex? mutex-clause)
  (is-a? mutex-clause mutex%))

(define (time-sleep t)
  (if (time-struct? t)
      (sfSleep t)
      (printf "expected _sfTime, got ~a" t)))

; ---------------------------------------------------------------------------------------------
; Thread.hpp
; ---------------------------------------------------------------------------------------------

(define sfml-thread%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make p)
      (when (and (not ptr)
                 p)
        (set! ptr (sfThread_create p))))

    (define/public (kill)
      (when ptr
        (sfThread_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfThread*? p)
        (send this kill)
        (set! ptr p)))
    
    (define/public (launch)
      (when ptr
        (sfThread_launch ptr)))

    (define/public (wait)
      (when ptr
        (sfThread_wait ptr)))

    (define/public (terminate)
      (when ptr
        (sfThread_terminate ptr)))))

(define (sfml-thread? thread-clause)
  (is-a? thread-clause sfml-thread%))
