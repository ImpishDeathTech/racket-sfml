#lang racket
(require csfml
         racket/generic
         "system.rkt")

(provide (all-defined-out))

; ---------------------------------------------------------------
; BlendMode.hpp
; ---------------------------------------------------------------

(define blend-mode make-sfBlendMode)
(define blend-mode? sfBlendMode?)

(define blend-mode-hash
  (hasheq 'color (hasheq 'src sfBlendMode-colorSrcFactor
                         'dst sfBlendMode-colorDstFactor
                         'equation sfBlendMode-colorEquation)
          'alpha (hasheq 'src sfBlendMode-alphaSrcFactor
                         'dst sfBlendMode-alphaDstFactor
                         'equation sfBlendMode-alphaEquation)))

(define (blend-mode-ref mode catigory value)
  ((hash-ref (hash-ref blend-mode-hash catigory) value) mode))

(define blend-ref blend-mode-ref)
(define blend-alpha sfBlendAlpha)
(define blend-add sfBlendAdd)
(define blend-mul sfBlendMultiply)
(define blend-none sfBlendNone)

(define blend-color-src-factor sfBlendMode-colorSrcFactor)
(define blend-color-dst-factor sfBlendMode-colorDstFactor)
(define blend-color-equation sfBlendMode-colorEquation)
(define blend-alpha-src-factor sfBlendMode-alphaSrcFactor)
(define blend-alpha-dst-factor sfBlendMode-alphaDstFactor)
(define blend-alpha-equation sfBlendMode-alphaEquation)

; ---------------------------------------------------------------
; Drawable.hpp
; ---------------------------------------------------------------

(define drawable<%>
  (interface () draw show))

(define (drawable? object-clause)
  (is-a? object-clause drawable<%>))

; ---------------------------------------------------------------
; Transform.hpp
; ---------------------------------------------------------------



(define-generics transform-matrix-funcs
  (a00 transform-matrix-funcs)
  (a01 transform-matrix-funcs)
  (a02 transform-matrix-funcs)
  (a10 transform-matrix-funcs)
  (a11 transform-matrix-funcs)
  (a12 transform-matrix-funcs)
  (a20 transform-matrix-funcs)
  (a21 transform-matrix-funcs)
  (a22 transform-matrix-funcs)
  (a00-set! transform-matrix-funcs value)
  (a01-set! transform-matrix-funcs value)
  (a02-set! transform-matrix-funcs value)
  (a10-set! transform-matrix-funcs value)
  (a11-set! transform-matrix-funcs value)
  (a12-set! transform-matrix-funcs value)
  (a20-set! transform-matrix-funcs value)
  (a21-set! transform-matrix-funcs value)
  (a22-set! transform-matrix-funcs value))
  
(struct transform-matrix
  ([a00 #:mutable]
   [a01 #:mutable]
   [a02 #:mutable]
   [a10 #:mutable]
   [a11 #:mutable]
   [a12 #:mutable]
   [a20 #:mutable]
   [a21 #:mutable]
   [a22 #:mutable])
  #:methods gen:transform-matrix-funcs
  [(define (a00 transform-matrix)
     (exact->inexact (transform-matrix-a00 transform-matrix)))

   (define (a01 transform-matrix)
     (exact->inexact (transform-matrix-a01 transform-matrix)))
   
   (define (a02 transform-matrix)
     (exact->inexact (transform-matrix-a02 transform-matrix)))

   (define (a10 transform-matrix)
     (exact->inexact (transform-matrix-a10 transform-matrix)))

   (define (a11 transform-matrix)
     (exact->inexact (transform-matrix-a11 transform-matrix)))

   (define (a12 transform-matrix)
     (exact->inexact (transform-matrix-a12 transform-matrix)))

   (define (a20 transform-matrix)
     (exact->inexact (transform-matrix-a20 transform-matrix)))

   (define (a21 transform-matrix)
     (exact->inexact (transform-matrix-a21 transform-matrix)))

   (define (a22 transform-matrix)
     (exact->inexact (transform-matrix-a22 transform-matrix)))

   (define (a00-set! transform-matrix value)
     (set-transform-matrix-a00! transform-matrix (exact->inexact value)))

   (define (a01-set! transform-matrix value)
     (set-transform-matrix-a01! transform-matrix (exact->inexact value)))

   (define (a02-set! transform-matrix value)
     (set-transform-matrix-a02! transform-matrix (exact->inexact value)))

   (define (a10-set! transform-matrix value)
     (set-transform-matrix-a10! transform-matrix (exact->inexact value)))

   (define (a11-set! transform-matrix value)
     (set-transform-matrix-a11! transform-matrix (exact->inexact value)))

   (define (a12-set! transform-matrix value)
     (set-transform-matrix-a12! transform-matrix (exact->inexact value)))

   (define (a20-set! transform-matrix value)
     (set-transform-matrix-a20! transform-matrix (exact->inexact value)))
   
   (define (a21-set! transform-matrix value)
     (set-transform-matrix-a21! transform-matrix value))
   
   (define (a22-set! transform-matrix value)
     (set-transform-matrix-a22! transform-matrix value))]
  #:transparent)

(define (make-transform-matrix)
  (transform-matrix 1. 0. 0.
                    0. 1. 0.
                    0. 0. 1.))

(define transform-identity sfTransform_Identity)

(define transform%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (set! ptr (sfTransform_fromMatrix 1. 0. 0.
                                        0. 1. 0.
                                        0. 0. 1.)))

    (define/public (kill)
      (set! ptr #f))
    
    (define/public (make-from-matrix mtx)
      (if (transform-matrix? mtx)
          (set! ptr (sfTransform_fromMatrix (a00 mtx) (a01 mtx) (a02 mtx)
                                            (a10 mtx) (a11 mtx) (a12 mtx)
                                            (a20 mtx) (a21 mtx) (a22 mtx)))
          (set! ptr (sfTransform_fromMatrix 1. 0. 0.
                                            0. 1. 0.
                                            0. 0. 1.))))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfTransform? p)
        (set! ptr p)))
    
    (define/public (get-inverse)
      (if ptr
          (sfTransform_getInverse ptr)
          #f))

    (define/public (transform-point point)
      (when ptr
        (if (vector2f? point)
            (sfTransform_transformPoint ptr point)
            (vector2f 0 0))))

    (define/public (transform-rect rect)
      (when ptr
        (if (sfFloatRect? rect)
            (sfTransform_transformRect ptr rect)
            (sfTransform_fromMatrix 1. 0. 0.
                                    0. 1. 0.
                                    0. 0. 1.))))

    (define/public (combine tsfm)
      (when (and ptr
                 (is-a? tsfm transform%))
            (sfTransform_combine ptr (send tsfm pointer))))

    (define/public (translate x y)
      (if ptr
          (sfTransform_translate ptr
                                 (exact->inexact x)
                                 (exact->inexact y))
          #f))

    (define/public (rotate angle)
      (when ptr
        (sfTransform_rotate ptr (exact->inexact angle))))

    (define/public (rotate-with-center angle x y z)
      (when ptr
        (sfTransform_rotateWithCenter ptr
                                      (exact->inexact angle)
                                      (exact->inexact x)
                                      (exact->inexact y)
                                      (exact->inexact z))))

    (define/public (scale x y)
      (when ptr
        (sfTransform_scale ptr
                           (exact->inexact x)
                           (exact->inexact y))))

    (define/public (scale-with-center ox oy cx cy)
      (when ptr
        (sfTransform_scaleWithCenter ptr ox oy cx cy)))

    (define/public (is-equal? tsfm)
      (if (and ptr
               (sfTransform? tsfm))
          (sfTransform_equal ptr tsfm)
          #f))))



(define (transform? object-clause)
  (is-a? object-clause transform%))

; ---------------------------------------------------------------
; Transformable.hpp
; ---------------------------------------------------------------

(define transformable<%>
  (interface ()
    set-position
    get-position
    set-rotation
    get-rotation
    set-scale
    get-scale
    set-origin
    get-origin
    move
    rotate
    scale
    get-transform
    get-inverse-transform))


(define (transformable? object-clause)
  (is-a? object-clause transformable<%>))


(define transformable%
  (class* object%
    (transformable<%>)
    (super-new)
    (field [tsfm-ptr (sfTransformable_create)])

    (define/public (make-transformable)
      (unless tsfm-ptr
        (set! tsfm-ptr (sfTransformable_create))))

    (define/public (kill-transformable)
      (when tsfm-ptr
        (sfTransformable_destroy tsfm-ptr)
        (set! tsfm-ptr #f)))

    (define/public (pointer) tsfm-ptr)

    (define/public (set-pointer p)
      (when (sfTransformable*? p)
        (send this kill)
        (set! tsfm-ptr p)))

    (define/public (copy)
      (define out (new transformable%))
      (when tsfm-ptr
        (send out set-pointer (sfTransformable_copy tsfm-ptr)))
      out)

    (define/public (set-position position)
      (when tsfm-ptr
        (cond [(vector2f? position)
               (sfTransformable_setPosition tsfm-ptr position)]
              [(pair? position)
               (sfTransformable_setPosition tsfm-ptr (pair->vector2f position))]
              [(or (vector2i? position)
                   (vector2u? position))
               (sfTransformable_setPosition tsfm-ptr (vector2i->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-rotation angle)
      (when tsfm-ptr
        (sfTransformable_setRotation tsfm-ptr (exact->inexact angle))))

    (define/public (set-scale factor)
      (when tsfm-ptr
        (cond [(vector2f? factor)
               (sfTransformable_setScale tsfm-ptr factor)]
              [(pair? factor)
               (sfTransformable_setScale tsfm-ptr (pair->vector2f factor))]
              [(or (vector2i? factor)
                   (vector2f? factor))
               (sfTransformable_setScale tsfm-ptr (vector2i->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (set-origin offset)
      (when tsfm-ptr
        (cond [(vector2f? offset)
               (sfTransformable_setOrigin tsfm-ptr offset)]
              [(pair? offset)
               (sfTransformable_setOrigin tsfm-ptr (pair->vector2f offset))]
              [(or (vector2i? offset)
                   (vector2u? offset))
               (sfTransformable_setOrigin tsfm-ptr (vector2i->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (get-position)
      (if tsfm-ptr
          (sfTransformable_getPosition tsfm-ptr)
          #f))

    (define/public (get-rotation)
      (if tsfm-ptr
          (sfTransformable_getRotation tsfm-ptr)
          #f))

    (define/public (get-scale)
      (if tsfm-ptr
          (sfTransformable_getScale tsfm-ptr)
          #f))

    (define/public (get-origin)
      (if tsfm-ptr
          (sfTransformable_getOrigin tsfm-ptr)
          #f))

    (define/public (move offset)
      (when tsfm-ptr
        (cond [(vector2f? offset)
               (sfTransformable_move tsfm-ptr offset)]
              [(pair? offset)
               (sfTransformable_move tsfm-ptr (pair->vector2f offset))]
              [(or (vector2i? offset)
                   (vector2f? offset))
               (sfTransformable_move tsfm-ptr (vector2i->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (rotate angle)
      (when tsfm-ptr
        (sfTransformable_rotate tsfm-ptr (exact->inexact angle))))

    (define/public (scale factor)
      (when tsfm-ptr
        (cond [(vector2f? factor)
               (sfTransformable_scale tsfm-ptr factor)]
              [(pair? factor)
               (sfTransformable_scale tsfm-ptr (pair->vector2f factor))]
              [(or (vector2i? factor)
                   (vector2u? factor))
               (sfTransformable_scale tsfm-ptr (vector2i->vector2f factor))]
              [else (printf "type-mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (get-transform)
      (define out (new transform%))
      (when tsfm-ptr
        (send out set-pointer (sfTransformable_getTransform tsfm-ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when tsfm-ptr
        (send out set-pointer (sfTransformable_getInverseTransform tsfm-ptr)))
      out)))

; ----------------------------------------------------------------------------------------------
; RenderStates.hpp
; ----------------------------------------------------------------------------------------------

(define-generics render-states-funcs
  (states->pointer render-states-funcs)
  (states-blend render-states-funcs)
  (states-transform render-states-funcs)
  (states-texture render-states-funcs)
  (states-shader render-states-funcs)
  (set-states-blend! render-states-funcs blend)
  (set-states-transform! render-states-funcs transform)
  (set-states-texture! render-states-funcs texture)
  (set-states-shader! render-states-funcs shader))

(struct render-states ([blend-mode #:mutable]
                       [transform #:mutable]
                       [texture #:mutable]
                       [shader #:mutable])
  #:methods gen:render-states-funcs
  [(define (states->pointer render-states)
     (make-sfRenderStates (render-states-blend-mode render-states)
                          (send (render-states-transform render-states) pointer)
                          (render-states-texture render-states)
                          (render-states-shader render-states)))

   (define (states-blend render-states)
     (render-states-blend-mode render-states))
   
   (define (states-transform render-states)
     (render-states-transform render-states))

   (define (states-texture render-states)
     (render-states-texture render-states))

   (define (states-shader render-states)
     (render-states-shader render-states))

   (define (set-states-blend! render-states blend)
     (set-render-states-blend-mode! render-states blend))
   
   (define (set-states-transform! render-states transform)
     (set-render-states-transform! render-states transform))

   (define (set-states-texture! render-states texture)
     (set-render-states-texture! render-states texture))

   (define (set-states-shader! render-states shader)
     (set-render-states-shader! render-states shader))])

(define (make-render-states)
  (define out (render-states sfBlendAlpha
                             (new transform%)
                             #f
                             #f))
  (send (states-transform out) make-from-matrix (make-transform-matrix))
  out)
                 

; ----------------------------------------------------------------------------------------------
; Color.hpp
; ----------------------------------------------------------------------------------------------

(define color? sfColor?)

(define (colorRGB r g b)
  (sfColor_fromRGB (abs (inexact->exact (round r)))
                   (abs (inexact->exact (round g)))
                   (abs (inexact->exact (round b)))))

(define (colorRGBA r g b a)
  (sfColor_fromRGBA (abs (inexact->exact (round r)))
                (abs (inexact->exact (round g)))
                (abs (inexact->exact (round b)))
                (abs (inexact->exact (round a)))))

(define (integer->color num)
  (if (integer? num)
      (sfColor_fromInteger (round num))
      #f))

(define (color->integer color-clause)
  (if (color? color-clause)
      (sfColor_toInteger color-clause)
      0))

(define-syntax add-colors
  (syntax-rules ()
    [(_ color-clause stx-clause ...)
       (let lp ([ls (list stx-clause ...)]
                [out color-clause])
         (when (not (eq? ls null))
           (set! out (lp (cdr ls) (sfColor_add color-clause (car ls)))))
         out)]))

(define-syntax sub-colors
  (syntax-rules ()
     [(_ color-clause stx-clause ...)
       (let lp ([ls (list stx-clause ...)]
                [out color-clause])
         (when (not (eq? ls null))
           (set! out (lp (cdr ls) (sfColor_subtract color-clause (car ls)))))
         out)]))

(define-syntax mod-colors
   (syntax-rules ()
     [(_ color-clause stx-clause ...)
       (let lp ([ls (list stx-clause ...)]
                [out color-clause])
         (when (not (eq? ls null))
           (set! out (lp (cdr ls) (sfColor_modulate color-clause (car ls)))))
         out)]))
     
      
(define color-r sfColor-r)
(define color-g sfColor-g)
(define color-b sfColor-b)
(define color-a sfColor-a)

(define set-color-r! set-sfColor-r!)
(define set-color-g! set-sfColor-g!)
(define set-color-b! set-sfColor-b!)
(define set-color-a! set-sfColor-a!)

(define-syntax color
  (syntax-rules ()
    [(_ stx ...)
     (let ([color-list (list stx ...)])
       (cond [(= (length color-list) 3)
              (colorRGB (car color-list)
                        (cadr color-list)
                        (caddr color-list))]
             [(= (length color-list) 4)
              (colorRGBA (car color-list)
                         (cadr color-list)
                         (caddr color-list)
                         (cadddr color-list))]
             [else (printf "type mismatch: expected 3-4, got ~a" (length color-list))]))]
    [(_)
     (colorRGB 0 0 0)]))

(define (list->color lst)
  (if (list? lst)
      (cond [(= (length lst) 3)
             (color (car lst)
                    (cadr lst)
                    (caddr lst))]
            [(= (length lst) 4)
             (color (car lst)
                    (cadr lst)
                    (caddr lst)
                    (cadddr lst))]
            [else (printf "type mismatch: expected list length 3-4, got ~a" (length lst))
                  #f])
      #f))

(define (color->list color-clause)
  (if (color? color-clause)
      (list (color-r color-clause)
            (color-g color-clause)
            (color-b color-clause)
            (color-a color-clause))
      #f))

(define (struct:color r g b a)
  (make-sfColor (abs (inexact->exact (truncate r)))
                (abs (inexact->exact (truncate g)))
                (abs (inexact->exact (truncate b)))
                (abs (inexact->exact (truncate a)))))

(define (vector->color vec)
  (if (and (vector? vec)
           (eq? (vector-ref vec 1) 'struct:color))
      (cond [(= (vector-length vec) 4)
             (set! vec (vector-append vec (vector 255)))
             (eval (vector->list vec))]
            [(= (vector-length vec) 5)
             (eval (vector->list vec))]
            [else (printf "type mismatch: expected length of 3-4, got ~a" (vector-length vec))
                  #f])
      #f))

(define (color->vector color-clause)
  (if (color? color-clause)
      (vector 'struct:color
              (color-r color-clause)
              (color-g color-clause)
              (color-b color-clause)
              (color-a color-clause))
      #f))

(define (hash->color hs)
  (if (hash? hs)
      (cond [(and (hash-has-key? hs 'r)
                  (and (hash-has-key? hs 'g)
                       (and (hash-has-key? hs 'b))))
             (if (hash-has-key? hs 'a)
                 (color (hash-ref hs 'r)
                        (hash-ref hs 'g)
                        (hash-ref hs 'b)
                        (hash-ref hs 'a))
                 (color (hash-ref hs 'r)
                        (hash-ref hs 'g)
                        (hash-ref hs 'b)))]
            [else (printf "missing key: mandatory keys: 'r 'g 'b, optional keys: 'a")
                  #f])
      #f))

(define (color->hash color-clause)
  (define out (make-hasheq))
  (hash-set*! out
              'r (color-r color-clause)
              'g (color-g color-clause)
              'b (color-b color-clause)
              'a (color-a color-clause))
  out)
; ---------------------------------------------------------------------------------------------
; Rect.hpp
; ---------------------------------------------------------------------------------------------

(define intrect? sfIntRect?)
(define florect? sfFloatRect?)
(define int-rect? sfIntRect?)
(define float-rect? sfFloatRect?)

(define (intrect l t w h)
  (make-sfIntRect (inexact->exact (truncate l))
                  (inexact->exact (truncate t))
                  (inexact->exact (truncate w))
                  (inexact->exact (truncate h))))

(define int-rect intrect)

(define (florect l t w h)
  (make-sfFloatRect (exact->inexact l)
                    (exact->inexact t)
                    (exact->inexact w)
                    (exact->inexact h)))

(define float-rect florect)

(define (list->intrect lst)
  (if (list? lst)
      (intrect (car lst)
               (cadr lst)
               (caddr lst)
               (cadddr lst))
      #f))

(define list->int-rect list->intrect)

(define (list->florect lst)
  (if (list? lst)
      (florect (car lst)
               (cadr lst)
               (caddr lst)
               (cadddr lst))
      #f))

(define florect-left sfFloatRect-left)
(define florect-top sfFloatRect-top)
(define florect-width sfFloatRect-width)
(define florect-height sfFloatRect-height)

(define intrect-left sfIntRect-left)
(define intrect-top sfIntRect-top)
(define intrect-width sfIntRect-width)
(define intrect-height sfIntRect-height)

(define (rect-left rect)
  (cond [(intrect? rect)
         (sfIntRect-left rect)]
        [(florect? rect)
         (sfFloatRect-left rect)]
        [else #f]))

(define (rect-top rect)
  (cond [(intrect? rect)
         (sfIntRect-top rect)]
        [(florect? rect)
         (sfFloatRect-top rect)]
        [else #f]))

(define (rect-width rect)
  (cond [(intrect? rect)
         (sfIntRect-width rect)]
        [(florect? rect)
         (sfFloatRect-width rect)]
        [else #f]))

(define (rect-height rect)
  (cond [(intrect? rect)
         (sfIntRect-height rect)]
        [(florect? rect)
         (sfFloatRect-height rect)]
        [else #f]))

(define (rect->list rect)
  (if (or (intrect? rect)
          (florect? rect))
      (list (rect-left rect)
            (rect-top rect)
            (rect-width rect)
            (rect-height rect))
      null))

(define (intrect-contains*? rect x y)
  (if (intrect? rect)
      (sfIntRect_contains rect
                          (inexact->exact (truncate x))
                          (inexact->exact (truncate y)))
      #f))

(define (intrect-contains? rect v2)
  (if (vector2i? v2)
      (intrect-contains*? rect
                         (vector2i-x v2)
                         (vector2i-y v2))
      #f))

(define (florect-contains*? rect x y)
  (if (florect? rect)
      (sfFloatRect_contains rect
                            (exact->inexact x)
                            (exact->inexact y))
      #f))

(define (florect-contains? rect point)
  (if (vector2f? point)
      (florect-contains*? rect
                        (vector2f-x point)
                        (vector2f-y point))
      #f))

(define-syntax rect-contains?
  (syntax-rules ()
    [(_ rect-clause x-clause y-clause)
     (cond [(intrect? rect-clause)
            (intrect-contains*? rect-clause x-clause y-clause)]
           [(florect? rect-clause)
            (florect-contains*? rect-clause x-clause y-clause)]
           [else #f])]
    [(_ rect-clause v2-clause)
     (cond [(intrect? rect-clause)
            (intrect-contains? rect-clause v2-clause)]
           [(florect? rect-clause)
            (florect-contains? rect-clause v2-clause)]
           [else #f])]))

(define-syntax rect-intersects?
  (syntax-rules ()
    [(_ rect-a rect-b)
     (cond [(and (intrect? rect-a)
                 (intrect? rect-b))
            (sfIntRect_intersects rect-a rect-b #f)]
           [(and (florect? rect-a)
                 (florect? rect-b))
            (sfFloatRect_intersects rect-a rect-b #f)]
           [else #f])]
    [(_ rect-a rect-b rect-c)
     (cond [(and (intrect? rect-a)
                 (intrect? rect-b)
                 (intrect? rect-c))
            (sfIntRect_intersects rect-a rect-b rect-c)]
           [(and (florect? rect-a)
                 (florect? rect-b)
                 (florect? rect-c))
            (sfFloatRect_intersects rect-a rect-b rect-c)]
           [else #f])]))

; ---------------------------------------------------------------------------------------------
; Font.hpp
; ---------------------------------------------------------------------------------------------

(define font%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make-from-file path)
      (unless ptr
        (set! ptr (sfFont_createFromFile (string-append (path->string (current-directory)) path)))))

    (define/public (make-from-memory data size)
      (unless ptr
        (set! ptr (sfFont_createFromMemory data size))))

    (define/public (make-from-stream istream)
      (unless ptr
        (set! ptr (sfFont_createFromStream istream))))

    (define/public (kill)
      (when ptr
        (sfFont_destroy ptr)
        (set! ptr #f)))
    
    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfFont*? p)
        (send this kill)
        (set! ptr p)))
    
    (define/public (copy)
      (define out (new font%))
      (when ptr
        (send out set-pointer (sfFont_copy ptr)))
      out)

    (define/public (get-glyph code-point character-size bold)
      (if ptr
          (sfFont_getGlyph ptr
                           (abs (truncate code-point))
                           (abs (truncate character-size))
                           bold)
          #f))

    (define/public (get-kerning first second character-size)
      (if ptr
          (sfFont_getKerning ptr
                             (abs (truncate first))
                             (abs (truncate second))
                             (abs (truncate character-size)))
          #f))

    (define/public (get-line-spacing character-size)
      (if ptr
          (sfFont_getLineSpacing ptr (abs (truncate character-size)))
          #f))

    (define/public (get-underline-position character-size)
      (if ptr
          (sfFont_getUnderlinePosition ptr (abs (truncate character-size)))
          #f))

    (define/public (get-underline-thickness character-size)
      (if ptr
          (sfFont_getUnderlineThickness ptr (abs (truncate character-size)))
          #f))

    (define/public (get-texture character-size)
      (if ptr
          (sfFont_getTexture ptr (abs (truncate character-size)))
          #f))

    (define/public (get-info)
      (if ptr
          (sfFont_getInfo ptr)
          #f))))

(define (font? object-clause)
  (is-a? object-clause font%))

; ---------------------------------------------------------------------------------------------
; Image.hpp
; ---------------------------------------------------------------------------------------------

(define image%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make x y)
      (unless ptr
        (set! ptr (sfImage_create (abs (truncate x))
                                  (abs (truncate y))))))

    (define/public (make-from-color x y new-color)
      (unless ptr
        (if (color? new-color)
            (set! ptr (sfImage_createFromColor (abs (truncate x))
                                               (abs (truncate y))
                                               new-color))
            (set! ptr (sfImage_createFromColor (abs (truncate x))
                                               (abs (truncate y))
                                               sfBlack)))))

    (define/public (make-from-pixels x y pixels)
      (unless ptr
        (set! ptr (sfImage_createFromPixels (abs (truncate x))
                                            (abs (truncate y))
                                            pixels))))

    (define/public (make-from-file path)
      (unless ptr
        (set! ptr (sfImage_createFromFile path))))

    (define/public (make-from-memory data size)
      (unless ptr
        (set! ptr (sfImage_createFromMemory data (abs (truncate size))))))

    (define/public (make-from-stream istream)
      (unless ptr
        (set! ptr (sfImage_createFromStream istream))))

    (define/public (kill)
      (when ptr
        (sfImage_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfImage*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (copy)
      (define out (new image%))
      (when ptr
        (send out set-pointer (sfImage_copy ptr)))
      out)

    (define/public (save-to-file path)
      (when ptr
        (sfImage_saveToFile ptr path)))

    (define/public (get-size)
      (if ptr
          (sfImage_getSize ptr)
          #f))

    (define/public (make-mask-from-color col)
      (when ptr
        (cond [(color? col)
               (sfImage_createMaskFromColor ptr col)]
              [(vector? col)
               (sfImage_createMaskFromColor ptr (vector->color col))]
              [(list? col)
               (sfImage_createMaskFromColor ptr (list->color col))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" col)])))

    (define/public (copy-image source dest-x dest-y source-rect apply-alpha)
      (define out (new image%))
      (when ptr
        (send out set-pointer
              (sfImage_copyImage ptr source dest-x dest-y source-rect apply-alpha)))
      out)

    (define/public (set-pixel x y col)
      (when ptr
        (if (color? col)
            (sfImage_setPixel ptr
                              (abs (truncate x))
                              (abs (truncate y))
                              col)
            (sfImage_setPixel ptr
                              (abs (truncate x))
                              (abs (truncate y))
                              sfBlack))))

    (define/public (get-pixel x y)
      (if ptr
          (sfImage_getPixel ptr
                            (abs (truncate x))
                            (abs (truncate y)))
          #f))

    (define/public (get-pixels-ptr)
      (if ptr
          (sfImage_getPixelsPtr ptr)
          #f))

    (define/public (flip-horizontally)
      (if ptr
          (sfImage_flipHorizontally ptr)
          #f))

    (define/public (flip-vertically)
      (when ptr
        (sfImage_flipVertically ptr)))

    (define/public (flip-h)
      (send this flip-horizontally))

    (define/public (flip-v)
      (send this flip-vertically))))
                          

(define (image? object-clause)
  (is-a? object-clause image%))

; ---------------------------------------------------------------------------------------------
; Texture.hpp
; ---------------------------------------------------------------------------------------------

(define texture%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make x y)
      (unless ptr
        (set! ptr (sfTexture_create (abs (inexact->exact (truncate x)))
                                    (abs (inexact->exact (truncate y)))))))

    (define/public (make-from-file path)
      (unless ptr
        (set! ptr (sfTexture_createFromFile path))))

    (define/public (make-from-memory data size)
      (unless ptr
        (set! ptr (sfTexture_createFromMemory data size))))

    (define/public (make-from-stream istream)
      (unless ptr
        (set! ptr (sfTexture_createFromStream istream))))

    (define/public (make-from-image img)
      (unless ptr
        (when (image? img)
          (set! ptr (sfTexture_createFromImage (send img pointer))))))

    (define/public (kill)
      (when ptr
        (sfTexture_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfTexture*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (copy)
      (define out (new texture%))
      (when ptr
        (send out set-pointer (sfTexture_copy ptr)))
      out)

    (define/public (get-size)
      (if ptr
          (sfTexture_getSize ptr)
          #f))

    (define/public (copy-to-image)
      (define out (new image%))
      (when ptr
        (send out set-pointer (sfTexture_copyToImage ptr)))
      out)

    (define/public (update-from-pixels pixels width height x y)
      (when ptr
        (sfTexture_updateFromPixels ptr
                                    pixels
                                    (abs (inexact->exact (truncate width)))
                                    (abs (inexact->exact (truncate height)))
                                    (abs (inexact->exact (truncate x)))
                                    (abs (inexact->exact (truncate y))))))

    (define/public (update-from-image img x y)
      (when (and ptr
                 (image? img))
        (sfTexture_updateFromImage ptr
                                   (send img pointer)
                                   (abs (floor (inexact->exact x)))
                                   (abs (floor (inexact->exact y))))))

    (define/public (update-from-render-window window x y)
      (when ptr
        (sfTexture_updateFromRenderWindow ptr
                                          (send window pointer)
                                          (abs (inexact->exact (truncate x)))
                                          (abs (inexact->exact (truncate y))))))

    (define/public (set-smooth flag)
      (when ptr
        (if (boolean? flag)
            (sfTexture_setSmooth ptr flag)
            (sfTexture_setSmooth ptr #f))))

    (define/public (is-smooth?)
      (if ptr
          (sfTexture_isSmooth ptr)
          #f))

    (define/public (set-srgb srgb)
      (when (and ptr
                 (boolean? srgb))
        (sfTexture_setSrgb ptr srgb)))

    (define/public (is-srgb?)
      (if ptr
          (sfTexture_isSrgb ptr)
          #f))

    (define/public (set-repeated flag)
      (when (and ptr
                 (boolean? flag))
        (sfTexture_setRepeated ptr flag)))

    (define/public (is-repeated?)
      (if ptr
          (sfTexture_isRepeated ptr)
          #f))

    (define/public (generate-mipmap)
      (when ptr
        (sfTexture_generateMipmap ptr)))

    (define/public (swap txr)
      (when (and ptr
                 (is-a? txr texture%))
        (sfTexture_swap ptr txr)))

    (define/public (get-native-handle)
      (if ptr
          (sfTexture_getNativeHandle ptr)
          #f))

    (define/public (bind)
      (when ptr
        (sfTexture_bind ptr)))

    (define/public (get-maximum-size)
      (if ptr
          (sfTexture_getMaximumSize ptr)
          #f))))

(define (texture? object-clause)
  (is-a? object-clause texture%))

; ---------------------------------------------------------------------------------------------
; View.hpp
; ---------------------------------------------------------------------------------------------

(define view%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfView_create))))

    (define/public (kill)
      (when ptr
        (sfView_destroy ptr)
        (set! ptr #f)))

    (define/public (make-from-rect rect)
      (when (and (not ptr)
                 (florect? rect))
        (set! ptr (sfView_createFromRect rect))))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfView*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (copy)
      (define out (new view%))
      (when ptr
        (send out set-pointer (sfView_copy ptr)))
      out)

    (define/public (set-center center)
      (when ptr
        (cond [(vector2f? center)
               (sfView_setCenter ptr center)]
              [(pair? center)
               (sfView_setCenter ptr (pair->vector2f center))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" center)])))

    (define/public (set-size size)
      (when ptr
        (cond [(vector2f? size)
               (sfView_setSize ptr size)]
              [(pair? size)
               (sfView_setSize ptr (pair->vector2f size))]
              [else "type mismatch: expected vector2f or pair, got ~a" size])))

    (define/public (set-rotation angle)
      (when ptr
        (sfView_setRotation ptr (exact->inexact angle))))

    (define/public (set-viewport port-rect)
      (when (and ptr
                 (florect? port-rect))
        (sfView_setViewport ptr port-rect)))

    (define/public (reset rect)
      (when (and ptr
                 (florect? rect))
        (sfView_reset ptr rect)))

    (define/public (get-center)
      (if ptr
          (sfView_getCenter ptr)
          #f))

    (define/public (get-rotation)
      (if ptr
          (sfView_getRotation ptr)
          #f))

    (define/public (get-viewport)
      (if ptr
          (sfView_getViewport ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfView_move ptr offset)]
              [(pair? offset)
               (sfView_move ptr (pair->vector2f ptr))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (rotate angle)
      (when ptr
        (sfView_rotate ptr (exact->inexact angle))))

    (define/public (zoom percentage)
      (when ptr
        (sfView_zoom ptr (exact->inexact angle))))))

(define (view? object-clause)
  (is-a? object-clause view%))

; ---------------------------------------------------------------------------------------------
; Vertex.hpp
; ---------------------------------------------------------------------------------------------

(define (make-vertex)
  (make-sfVertex (vector2f 0 0)
                 sfWhite
                 (vector2f 0 0)))
        
(define vertex make-sfVertex)
(define vertex? sfVertex?)
(define vertex-position sfVertex-position)
(define vertex-color sfVertex-color)
(define vertex-tex-coords sfVertex-texCoords)
(define set-vertex-position! set-sfVertex-position!)
(define set-vertex-color! set-sfVertex-color!)
(define set-vertex-tex-coords! set-sfVertex-texCoords!)

; ---------------------------------------------------------------------------------------------
; VertexArray.hpp
; ---------------------------------------------------------------------------------------------

(define vertex-array%
  (class* object%
    (drawable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfVertexArray_create))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfVertexArray_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfVertexArray*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))

    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawVertexArray target ptr states)))
    
    (define/public (show flag)
      (when (and ptr
                 (boolean? flag))
        (set! show? flag)))

    (define/public (copy)
      (define out (new vertex-array%))
      (when ptr
        (send out set-pointer (sfVertexArray_copy ptr)))
      out)

    (define/public (get-vertex-count)
      (if ptr
          (sfVertexArray_getVertexCount ptr)
          #f))

    (define/public (get-vertex index)
      (when ptr
        (if (exact-nonnegative-integer? index)
            (sfVertexArray_getVertex ptr index)
            (sfVertexArray_getVertex ptr (abs (inexact->exact (truncate index)))))))

    (define/public (clear)
      (when ptr
        (sfVertexArray_clear ptr)))

    (define/public (resize size)
      (when ptr
        (if (exact-nonnegative-integer? size)
            (sfVertexArray_resize ptr size)
            (sfVertexArray_resize ptr (abs (inexact->exact (truncate size)))))))

    (define/public (append vtx)
      (when (and ptr
                 (vertex? vtx))
        (sfVertexArray_append ptr vtx)))

    (define/public (set-primitive-type primitive)
      (if ptr
          (sfVertexArray_setPrimitiveType ptr primitive)
          #f))

    (define/public (get-bounds)
      (if ptr
          (sfVertexArray_getBounds ptr)
          #f))))
        
(define (vertex-array? object-clause)
  (is-a? object-clause vertex-array%))

; ---------------------------------------------------------------------------------------------
; VertexBuffer.hpp
; ---------------------------------------------------------------------------------------------

(define vertex-buffer%
  (class* object%
    (drawable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make size primitive usage)
      (unless ptr
        (set! ptr (sfVertexBuffer_create size primitive usage))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfVertexBuffer_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfVertexBuffer*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))

    (define/public (draw target states)
      (when ptr
        (sfRenderWindow_drawVertexBuffer target ptr states)))

    (define/public (show flag)
      (when (and ptr
                 (boolean? flag))
        (set! show? flag)))

    (define/public (copy)
      (define out (new vertex-buffer%))
      (when ptr
        (send out set-pointer (sfVertexBuffer_copy ptr)))
      out)

    (define/public (get-vertex-count)
      (if ptr
          (sfVertexBuffer_getVertexCount ptr)
          #f))

    (define/public (update vertices vertex-count offset)
      (when ptr
              (if (and (exact-nonnegative-integer? vertex-count)
                       (exact-nonnegative-integer? offset))
                  (sfVertexBuffer_update ptr vertices vertex-count offset)
                  (sfVertexBuffer_update ptr
                                         vertices
                                         (abs (inexact->exact (truncate vertex-count)))
                                         (abs (inexact->exact (truncate offset)))))))

    (define/public (swap vertex-buff)
      (when (and ptr
                 (is-a? vertex-buff vertex-buffer%))
        (sfVertexBuffer_swap ptr vertex-buff)))

    (define/public (get-native-handle)
      (if ptr
          (sfVertexBuffer_getNativeHandle ptr)
          #f))

    (define/public (set-primitive-type primitive)
      (when ptr
        (sfVertexBuffer_setPrimitiveType ptr primitive)))

    (define/public (get-primitive-type)
      (if ptr
          (sfVertexBuffer_getPrimitiveType ptr)
          #f))

    (define/public (set-usage usage)
      (when ptr
        (sfVertexBuffer_setUsage ptr usage)))

    (define/public (get-usage)
      (if ptr
          (sfVertexBuffer_getUsage ptr)
          #f))

    (define/public (bind)
      (when ptr
        (sfVertexBuffer_bind ptr)))

    (define/public (is-available?)
      (if ptr
          (sfVertexBuffer_isAvailable ptr)
          #f))))

(define (vertex-buffer? object-clause)
  (is-a? object-clause vertex-buffer%))

; ---------------------------------------------------------------------------------------------
; CircleShape.hpp
; ---------------------------------------------------------------------------------------------

(define circle-shape%
  (class* object%
    (drawable<%>
     transformable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfCircleShape_create))
        (set! show? #t)))

    (define/public (make* r)
      (when (not ptr)
        (set! ptr (sfCircleShape_create))
        (sfCircleShape_setRadius ptr (exact->inexact r))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfCircleShape_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfCircleShape*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))

    (define/public (show flag)
      (if (boolean? flag)
          (set! show? flag)
          (printf "type mismatch: expected boolean, got ~a" flag)))

    (define/public (set-texture txr)
      (when (and ptr
                 (texture? txr))
        (sfCircleShape_setTexture ptr (send txr pointer) #f)))

    (define/public (set-texture* txr reset-rect)
      (when (and ptr
                 (texture? txr)
                 (boolean? reset-rect))
        (sfCircleShape_setTexture ptr txr reset-rect)))

    (define/public (get-texture)
      (define out (new texture%))
      (when ptr
          (send out set-pointer (sfCircleShape_getTexture ptr)))
      out)

    (define/public (set-texture-rect rect)
      (when (and ptr
                 (intrect? rect))
        (sfCircleShape_setTextureRect ptr rect)))

    (define/public (get-texture-rect)
      (if ptr
          (sfCircleShape_getTextureRect ptr)
          #f))
    
    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawCircleShape target ptr states)))
    
    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfCircleShape_setPosition ptr position)]
              [(pair? position)
               (sfCircleShape_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-position* x y)
      (send this set-position (vector2f x y)))
  
    (define/public (get-position)
      (if ptr
          (sfCircleShape_getPosition ptr)
          #f))

    (define/public (set-rotation radius)
      (when ptr
        (sfCircleShape_setRotation ptr (exact->inexact radius))))

    (define/public (get-rotation)
      (if ptr
          (sfCircleShape_getRotation ptr)
          #f))

    (define/public (set-scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfCircleShape_setScale ptr factor)]
              [(pair? factor)
               (sfCircleShape_setScale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (set-scale* x y)
      (send this set-scale (vector2f x y)))

    (define/public (get-scale)
      (if ptr
          (sfCircleShape_getScale ptr)
          #f))

    (define/public (set-origin offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfCircleShape_setOrigin ptr offset)]
              [(pair? offset)
               (sfCircleShape_setOrigin ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (set-origin* x y)
      (send this set-origin (vector2f x y)))

    (define/public (get-origin)
      (if ptr
          (sfCircleShape_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfCircleShape_move ptr offset)]
              [(pair? offset)
               (sfCircleShape_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (move* x y)
      (send this move (vector2f x y)))

    (define/public (scale factor)
      (cond [(vector2f? factor)
               (sfCircleShape_scale ptr factor)]
              [(pair? factor)
               (sfCircleShape_scale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)]))

    (define/public (scale* x y)
      (send this scale (vector2f x y)))

    (define/public (rotate radius)
      (when ptr
        (sfCircleShape_rotate (exact->inexact radius))))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfCircleShape_getTransform ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfCircleShape_getInverseTransform ptr)))
      out)

    (define/public (set-fill-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfCircleShape_setFillColor ptr color-clause)]
              [(list? color-clause)
               (sfCircleShape_setFillColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfCircleShape_setFillColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-fill-color)
      (if ptr
          (sfCircleShape_getFillColor ptr)
          #f))

    (define/public (set-outline-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfCircleShape_setOutlineColor ptr color-clause)]
              [(list? color-clause)
               (sfCircleShape_setOutlineColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfCircleShape_setOutlineColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-outline-color)
      (if ptr
          (sfCircleShape_getOutlineColor ptr)
          #f))

    (define/public (set-outline-thickness thickness)
      (when ptr
        (sfCircleShape_setOutlineThickness ptr (exact->inexact thickness))))

    (define/public (get-outline-thickness)
      (if ptr
          (sfCircleShape_getOutlineThickness ptr)
          #f))

    (define/public (set-point-count point-count)
      (when ptr
        (sfCircleShape_setPointCount ptr point-count)))
    
    (define/public (get-point-count)
      (if ptr
          (sfCircleShape_getPointCount ptr)
          #f))

    (define/public (set-radius radius)
      (when ptr
        (sfCircleShape_setRadius ptr (exact->inexact radius))))

    (define/public (get-radius)
      (if ptr
          (sfCircleShape_getRadius ptr)
          #f))

    (define/public (get-point index)
      (when ptr
        (sfCircleShape_getPoint ptr index)))

    (define/public (get-local-bounds)
      (if ptr
          (sfCircleShape_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfCircleShape_getGlobalBounds ptr)
          #f))))

(define (circle-shape? circle-clause)
  (is-a? circle-clause circle-shape%))

; -----------------------------------------------------------------------------------------------------------
; ConvexShape.hpp
; -----------------------------------------------------------------------------------------------------------

(define convex-shape%
  (class* object%
    (drawable<%>
     transformable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfConvexShape_create))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfConvexShape_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfConvexShape*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))
    
    (define/public (show flag)
      (if (boolean? flag)
          (set! show? flag)
          (printf "type mismatch: expected boolean, got ~a" flag)))
    
    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawConvexShape target ptr states)))

    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfConvexShape_setPosition ptr position)]
              [(pair? position)
               (sfConvexShape_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-position* x y)
      (send this set-position (vector2f x y)))
  
    (define/public (get-position)
      (if ptr
          (sfConvexShape_getPosition ptr)
          #f))

    (define/public (set-rotation radius)
      (when ptr
        (sfConvexShape_setRotation ptr (exact->inexact radius))))

    (define/public (get-rotation)
      (if ptr
          (sfConvexShape_getRotation ptr)
          #f))

    (define/public (set-scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfConvexShape_setScale ptr factor)]
              [(pair? factor)
               (sfConvexShape_setScale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (set-scale* x y)
      (send this set-scale (vector2f x y)))

    (define/public (get-scale)
      (if ptr
          (sfConvexShape_getScale ptr)
          #f))

    (define/public (set-origin offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfConvexShape_setOrigin ptr offset)]
              [(pair? offset)
               (sfConvexShape_setOrigin ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (set-origin* x y)
      (send this set-origin (vector2f x y)))

    (define/public (get-origin)
      (if ptr
          (sfConvexShape_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfConvexShape_move ptr offset)]
              [(pair? offset)
               (sfConvexShape_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (move* x y)
      (send this move (vector2f x y)))

    (define/public (scale factor)
      (cond [(vector2f? factor)
             (sfConvexShape_scale ptr factor)]
            [(pair? factor)
             (sfConvexShape_scale ptr (pair->vector2f factor))]
            [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)]))

    (define/public (scale* x y)
      (send this scale (vector2f x y)))

    (define/public (rotate radius)
      (when ptr
        (sfConvexShape_rotate (exact->inexact radius))))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfConvexShape_getTransform ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfConvexShape_getInverseTransform ptr)))
      out)

    (define/public (set-fill-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfConvexShape_setFillColor ptr color-clause)]
              [(list? color-clause)
               (sfConvexShape_setFillColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfConvexShape_setFillColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-fill-color)
      (if ptr
          (sfConvexShape_getFillColor ptr)
          #f))

    (define/public (set-outline-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfConvexShape_setOutlineColor ptr color-clause)]
              [(list? color-clause)
               (sfConvexShape_setOutlineColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfConvexShape_setOutlineColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-outline-color)
      (if ptr
          (sfConvexShape_getOutlineColor ptr)
          #f))

    (define/public (set-outline-thickness thickness)
      (if ptr
          (sfConvexShape_setOutlineThickness ptr (exact->inexact thickness))
          #f))

    (define/public (get-outline-thickness)
      (if ptr
          (sfConvexShape_getOutlineThickness ptr)
          #f))

    (define/public (set-point-count point-count)
      (when ptr
        (sfConvexShape_setPointCount ptr point-count)))
    
    (define/public (get-point-count)
      (if ptr
          (sfConvexShape_getPointCount ptr)
          #f))

    (define/public (get-point index)
      (if ptr
          (sfConvexShape_getPoint ptr index)
          #f))

    (define/public (set-point index point)
      (when ptr
        (cond [(vector2f? point)
               (sfConvexShape_setPoint ptr index point)]
              [(pair? point)
               (sfConvexShape_setPoint ptr index (pair->vector2f point))]
              [else (printf "type mismatch: expected vector2f, got ~a" point)])))

    (define/public (set-point* index x y)
      (send this set-point index (vector2f x y)))

    (define/public (get-local-bounds)
      (if ptr
          (sfConvexShape_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfConvexShape_getGlobalBounds ptr)
          #f))))

(define (convex-shape? convex-clause)
  (is-a? convex-clause convex-shape%))
    

; ------------------------------------------------------------------------------------------------
; RectangleShape.hpp
; ------------------------------------------------------------------------------------------------

(define rectangle-shape%
  (class* object%
    (drawable<%>
     transformable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfRectangleShape_create))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfRectangleShape_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfRectangleShape*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))
    
    (define/public (show flag)
      (if (boolean? flag)
          (set! show? flag)
          (printf "type mismatch: expected boolean, got ~a" flag)))
    
    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawRectangleShape target ptr states)))

    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfRectangleShape_setPosition ptr position)]
              [(pair? position)
               (sfRectangleShape_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-texture txr)
      (when (and ptr
                 (texture? txr))
        (sfRectangleShape_setTexture ptr (send txr pointer) #f)))

    (define/public (set-texture* txr reset-rect)
      (when (and ptr
                 (boolean? reset-rect)
                 (texture? txr))
        (sfRectangleShape_setTexture ptr (send txr pointer) reset-rect)))

    (define/public (get-texture)
      (define out (new texture%))
      (when ptr
          (send out set-pointer (sfRectangleShape_getTexture ptr)))
      out)

    (define/public (set-texture-rect rect)
      (when (and ptr
                 (intrect? rect))
        (sfRectangleShape_setTextureRect ptr rect)))

    (define/public (get-texture-rect)
      (if ptr
          (sfRectangleShape_getTextureRect ptr)
          #f))

    (define/public (set-position* x y)
      (send this set-position (vector2f x y)))
  
    (define/public (get-position)
      (if ptr
          (sfRectangleShape_getPosition ptr)
          #f))

    (define/public (set-rotation radius)
      (when ptr
        (sfRectangleShape_setRotation ptr (exact->inexact radius))))

    (define/public (get-rotation)
      (if ptr
          (sfRectangleShape_getRotation ptr)
          #f))

    (define/public (set-scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfRectangleShape_setScale ptr factor)]
              [(pair? factor)
               (sfRectangleShape_setScale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (set-scale* x y)
      (send this set-scale (vector2f x y)))

    (define/public (get-scale)
      (if ptr
          (sfRectangleShape_getScale ptr)
          #f))

    (define/public (set-origin offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfRectangleShape_setOrigin ptr offset)]
              [(pair? offset)
               (sfRectangleShape_setOrigin ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (set-origin* x y)
      (send this set-origin (vector2f x y)))

    (define/public (get-origin)
      (if ptr
          (sfRectangleShape_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfRectangleShape_move ptr offset)]
              [(pair? offset)
               (sfRectangleShape_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (move* x y)
      (send this move (vector2f x y)))

    (define/public (scale factor)
      (cond [(vector2f? factor)
               (sfRectangleShape_scale ptr factor)]
              [(pair? factor)
               (sfRectangleShape_scale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)]))

    (define/public (scale* x y)
      (send this scale (vector2f x y)))

    (define/public (rotate radius)
      (if ptr
          (sfRectangleShape_rotate (exact->inexact radius))
          #f))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfRectangleShape_getTransform ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfRectangleShape_getInverseTransform ptr)))
      out)

    (define/public (set-fill-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfRectangleShape_setFillColor ptr color-clause)]
              [(list? color-clause)
               (sfRectangleShape_setFillColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfRectangleShape_setFillColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-fill-color)
      (if ptr
          (sfRectangleShape_getFillColor ptr)
          #f))

    (define/public (set-outline-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfRectangleShape_setOutlineColor ptr color-clause)]
              [(list? color-clause)
               (sfRectangleShape_setOutlineColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfRectangleShape_setOutlineColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-outline-color)
      (if ptr
          (sfRectangleShape_getOutlineColor ptr)
          #f))

    (define/public (set-outline-thickness thickness)
      (if ptr
          (sfRectangleShape_setOutlineThickness ptr (exact->inexact thickness))
          #f))

    (define/public (get-outline-thickness)
      (if ptr
          (sfRectangleShape_getOutlineThickness ptr)
          #f))
    
    (define/public (get-point-count)
      (if ptr
          (sfRectangleShape_getPointCount ptr)
          #f))

    (define/public (get-point index)
      (when ptr
        (sfRectangleShape_getPoint ptr index)))

    (define/public (set-size size)
      (when ptr
        (cond [(vector2f? size)
               (sfRectangleShape_setSize ptr size)]
              [(pair? size)
               (sfRectangleShape_setSize ptr (pair->vector2f size))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" size)])))

    (define/public (get-size)
      (if ptr
          (sfRectangleShape_getSize ptr)
          #f))

    (define/public (get-local-bounds)
      (if ptr
          (sfRectangleShape_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfRectangleShape_getGlobalBounds ptr)
          #f))))

(define (rectangle-shape? object-clause)
  (is-a? object-clause rectangle-shape%))

; -------------------------------------------------------------------------------
; Shape.hpp
; -------------------------------------------------------------------------------

(define shape%
  (class* object%
    (drawable<%>
     transformable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make on-get-point-count on-get-point user-data)
      (when (not ptr)
        (set! ptr (sfShape_create on-get-point-count on-get-point user-data))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfShape_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfShape*? ptr)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))
    
    (define/public (show flag)
      (if (boolean? flag)
          (set! show? flag)
          (printf "type mismatch: expected boolean, got ~a" flag)))
    
    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawShape target ptr states)))
    
    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfShape_setPosition ptr position)]
              [(pair? position)
               (sfShape_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-position* x y)
      (send this set-position (vector2f x y)))
  
    (define/public (get-position)
      (if ptr
          (sfShape_getPosition ptr)
          #f))

    (define/public (set-rotation radius)
      (when ptr
        (sfShape_setRotation ptr (exact->inexact radius))))

    (define/public (get-rotation)
      (if ptr
          (sfShape_getRotation ptr)
          #f))

    (define/public (set-scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfShape_setScale ptr factor)]
              [(pair? factor)
               (sfShape_setScale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (set-scale* x y)
      (send this set-scale (vector2f x y)))

    (define/public (get-scale)
      (if ptr
          (sfShape_getScale ptr)
          #f))

    (define/public (set-origin offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfShape_setOrigin ptr offset)]
              [(pair? offset)
               (sfShape_setOrigin ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (set-origin* x y)
      (send this set-origin (vector2f x y)))

    (define/public (get-origin)
      (if ptr
          (sfShape_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfShape_move ptr offset)]
              [(pair? offset)
               (sfShape_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (move* x y)
      (send this move (vector2f x y)))

    (define/public (scale factor)
      (cond [(vector2f? factor)
               (sfShape_scale ptr factor)]
              [(pair? factor)
               (sfShape_scale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f pair, got ~a" factor)]))

    (define/public (scale* x y)
      (send this scale (vector2f x y)))

    (define/public (rotate radius)
      (when ptr
        (sfShape_rotate (exact->inexact radius))))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfShape_getTransform ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfShape_getInverseTransform ptr)))
      out)

    (define/public (set-fill-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfShape_setFillColor ptr color-clause)]
              [(list? color-clause)
               (sfShape_setFillColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfShape_setFillColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-fill-color)
      (if ptr
          (sfShape_getFillColor ptr)
          #f))

    (define/public (set-outline-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfShape_setOutlineColor ptr color-clause)]
              [(list? color-clause)
               (sfShape_setOutlineColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfShape_setOutlineColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-outline-color)
      (if ptr
          (sfShape_getOutlineColor ptr)
          #f))

    (define/public (set-outline-thickness thickness)
      (when ptr
        (sfShape_setOutlineThickness ptr (exact->inexact thickness))))

    (define/public (get-outline-thickness)
      (if ptr
          (sfShape_getOutlineThickness ptr)
          #f))
    
    (define/public (get-point-count)
      (if ptr
          (sfShape_getPointCount ptr)
          #f))

    (define/public (get-point index)
      (when ptr
        (sfShape_getPoint ptr index)))

    (define/public (get-local-bounds)
      (if ptr
          (sfShape_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfShape_getGlobalBounds ptr)
          #f))))

(define (shape? object-clause)
  (is-a? object-clause shape%))

; ---------------------------------------------------------------------------------------------
; Sprite.hpp
; ---------------------------------------------------------------------------------------------

(define sprite%
  (class* object%
    (drawable<%>
     transformable<%>)
    (super-new)
    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfSprite_create))
        (set! show? #t)))

    (define/public (kill)
      (when ptr
        (sfSprite_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfSprite*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))

    (define/public (show flag)
      (if (boolean? flag)
          (set! show? flag)
          (printf "type mismatch: expected boolean, got ~a" flag)))

    (define/public (set-texture txr)
      (when (and ptr
                 (texture? txr))
        (sfSprite_setTexture ptr txr #f)))

    (define/public (set-texture* txr reset-rect)
      (when (and ptr
                 (texture? txr)
                 (boolean? reset-rect))
        (sfSprite_setTexture ptr (send txr pointer) reset-rect)))

    (define/public (get-texture)
      (define out (new texture%))
      (when ptr
          (send out set-pointer (sfSprite_getTexture ptr)))
      out)

    (define/public (set-texture-rect rect)
      (when (and ptr
                 (intrect? rect))
        (sfSprite_setTextureRect ptr rect)))

    (define/public (get-texture-rect)
      (if ptr
          (sfSprite_getTextureRect ptr)
          #f))
    
    (define/public (draw target states)
      (when (and ptr
                 show?)
        (sfRenderWindow_drawSprite target ptr states)))
    
    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfSprite_setPosition ptr position)]
              [(pair? position)
               (sfSprite_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (set-position* x y)
      (send this set-position (vector2f x y)))
  
    (define/public (get-position)
      (if ptr
          (sfSprite_getPosition ptr)
          #f))

    (define/public (set-rotation radius)
      (when ptr
        (sfSprite_setRotation ptr (exact->inexact radius))))

    (define/public (get-rotation)
      (when ptr
        (sfSprite_getRotation ptr)))

    (define/public (set-scale factor)
      (if ptr
          (cond [(vector2f? factor)
                 (sfSprite_setScale ptr factor)]
                [(pair? factor)
                 (sfSprite_setScale ptr (pair->vector2f factor))]
                [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])
          #f))

    (define/public (set-scale* x y)
      (send this set-scale (vector2f x y)))

    (define/public (get-scale)
      (if ptr
          (sfSprite_getScale ptr)
          #f))

    (define/public (set-origin offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfSprite_setOrigin ptr offset)]
              [(pair? offset)
               (sfSprite_setOrigin ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))

    (define/public (set-origin* x y)
      (send this set-origin (vector2f x y)))

    (define/public (get-origin)
      (if ptr
          (sfSprite_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfSprite_move ptr offset)]
              [(pair? offset)
               (sfSprite_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f, got ~a" offset)])))

    (define/public (move* x y)
      (send this move (vector2f x y)))

    (define/public (scale factor)
      (cond [(vector2f? factor)
               (sfSprite_scale ptr factor)]
              [(pair? factor)
               (sfSprite_scale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f, got ~a" factor)]))

    (define/public (scale* x y)
      (send this scale (vector2f x y)))

    (define/public (rotate radius)
      (when ptr
        (sfSprite_rotate (exact->inexact radius))))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfSprite_getTransform ptr)))
      out)

    (define/public (get-inverse-transform)
      (define out (new transform%))
      (when ptr
        (send out set-pointer (sfSprite_getInverseTransform ptr)))
      out)

    (define/public (set-color color-clause)
      (when ptr
        (cond [(color? color-clause)
               (sfSprite_setColor ptr color-clause)]
              [(list? color-clause)
               (sfSprite_setColor ptr (list->color color-clause))]
              [(vector? color-clause)
               (sfSprite_setColor ptr (vector->color color-clause))]
              [else (printf "type mismatch: expected color, vector or list, got ~a" color-clause)])))

    (define/public (get-color)
      (if ptr
          (sfSprite_getColor ptr)
          #f))

    (define/public (get-local-bounds)
      (if ptr
          (sfSprite_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfSprite_getGlobalBounds ptr)
          #f))))

(define (sprite? object-clause)
  (is-a? object-clause sprite%))

; ----------------------------------------------------------------------------------------------------------------
; Text.hpp
; ----------------------------------------------------------------------------------------------------------------

(define text%
  (class* object%
    (drawable<%>
     transformable<%>)

    (field [ptr #f]
           [show? #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfText_create))
        (set! show? #t)))

    (define/public (make-from-font f)
      (when (and (not ptr)
                 (font? f))
        (send this make)
        (sfText_setFont ptr f)))

    (define/public (kill)
      (when ptr
        (sfText_destroy ptr)
        (set! ptr #f)
        (set! show? #f)))

    (define/public (copy)
      (define out (new text%))
      (when ptr
        (send out set-pointer (sfText_copy ptr)))
      out)

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfText*? p)
        (send this kill)
        (set! ptr p)
        (set! show? #t)))
    
    (define/public (draw target states)
      (when ptr
        (sfRenderWindow_drawText target ptr states)))

    (define/public (show flag)
      (when (and ptr
                 (boolean? flag))
        (set! show? flag)))
    
    (define/public (set-position position)
      (when ptr
        (cond [(vector2f? position)
               (sfText_setPosition ptr position)]
              [(pair? position)
               (sfText_setPosition ptr (pair->vector2f position))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" position)])))

    (define/public (get-position)
      (if ptr
          (sfText_getPosition ptr)
          #f))

    (define/public (set-rotation angle)
      (when ptr
        (sfText_setRotation ptr (exact->inexact angle))))

    (define/public (get-rotation)
      (if ptr
          (sfText_getRotation ptr)
          #f))

    (define/public (get-inverse-transform) (void))

    (define/public (set-scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfText_setScale ptr factor)]
              [(pair? factor)
               (sfText_setScale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (get-scale)
      (if ptr
          (sfText_getScale ptr)
          #f))

    (define/public (get-origin)
      (if ptr
          (sfText_getOrigin ptr)
          #f))

    (define/public (move offset)
      (when ptr
        (cond [(vector2f? offset)
               (sfText_move ptr offset)]
              [(pair? offset)
               (sfText_move ptr (pair->vector2f offset))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" offset)])))
    
    (define/public (scale factor)
      (when ptr
        (cond [(vector2f? factor)
               (sfText_scale ptr factor)]
              [(pair? factor)
               (sfText_scale ptr (pair->vector2f factor))]
              [else (printf "type mismatch: expected vector2f or pair, got ~a" factor)])))

    (define/public (rotate angle)
      (when ptr
        (sfText_rotate ptr (exact->inexact angle))))

    (define/public (get-transform)
      (define out (new transform%))
      (when ptr
          (send out set-pointer (sfText_getTransform ptr)))
      out)

    (define/public (set-string str)
      (when ptr
        (sfText_setString ptr str)))

    (define/public (get-string)
      (if ptr
          (sfText_getString ptr)
          #f))

    (define/public (set-font f)
      (when (and ptr
                 (font? f))
        (sfText_setFont ptr (send f pointer))))
    
    (define/public (set-origin a) (void))

    (define/public (get-font)
      (define out (new font%))
      (when ptr
        (send out set-pointer (sfText_getFont ptr)))
      out)

    (define/public (set-character-size size)
      (when ptr
        (sfText_setCharacterSize ptr (exact->inexact size))))

    (define/public (set-line-spacing spacing)
      (when ptr
        (sfText_setLineSpacing ptr (exact->inexact spacing))))

    (define/public (get-line-spacing)
      (if ptr
          (sfText_getLineSpacing ptr)
          #f))

    (define/public (set-letter-spacing spacing)
      (when ptr
        (sfText_setLetterSpacing ptr (exact->inexact spacing))))

    (define/public (get-letter-spacing)
      (if ptr
          (sfText_getLetterSpacing ptr)
          #f))

    (define/public (set-style style)
      (when ptr
        (sfText_setStyle ptr (inexact->exact style))))

    (define/public (set-fill-color color-clause)
      (when (and ptr
                 (color? color-clause))
        (sfText_setFillColor ptr color-clause)))

    (define/public (get-fill-color)
      (if ptr
          (sfText_getFillColor ptr)
          #f))

    (define/public (set-outline-color color-clause)
      (when (and ptr
                 (color? color-clause))
        (sfText_setOutlineColor ptr color-clause)))

    (define/public (get-outline-color)
      (if ptr
          (sfText_getOutlineColor ptr)
          #f))

    (define/public (set-outline-thickness thickness)
      (when ptr
        (sfText_setOutlineThickness ptr (exact->inexact thickness))))

    (define/public (get-outline-thickness)
      (if ptr
          (sfText_getOutlineThickness ptr)
          #f))

    (define/public (find-character-pos pos)
      (when ptr
        (sfText_findCharacterPos ptr (abs (floor (inexact->exact pos))))))

    (define/public (get-local-bounds)
      (if ptr
          (sfText_getLocalBounds ptr)
          #f))

    (define/public (get-global-bounds)
      (if ptr
          (sfText_getGlobalBounds ptr)
          #f))))

(define (text? object-clause)
  (is-a? object-clause text%))
        
        
; --------------------------------------------------------------------------------------------------------------
; RenderTexture.hpp
; --------------------------------------------------------------------------------------------------------------

(define render-texture%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make width height depth-buffer)
      (unless ptr
        (set! ptr (sfRenderTexture_create width height depth-buffer))))

    (define/public (kill)
      (when ptr
        (sfRenderTexture_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfRenderTexture*? p)
        (send this kill)
        (set! ptr p)))
    
    (define/public (get-size)
      (when ptr
        (sfRenderTexture_getSize ptr)))

    (define/public (set-active flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderTexture_setActive ptr flag)))

    (define/public (clear)
      (when ptr
        (sfRenderTexture_clear ptr sfBlack)))

    (define/public (clear* color-clause)
      (when (and ptr
                 (color? color-clause))
        (sfRenderTexture_clear ptr color-clause)))

    (define/public (set-view v)
      (when (and ptr
                 (view? v))
        (sfRenderTexture_setView ptr v)))

    (define/public (get-view)
      (define out (new view%))
      (when ptr
        (send out set-pointer (sfRenderTexture_getView ptr)))
      out)

    (define/public (get-default-view)
      (define out (new view%))
      (when ptr
        (send out set-pointer (sfRenderTexture_getDefaultView ptr))))

    (define/public (get-viewport v)
      (when (and ptr
                 (view? v))
        (sfRenderTexture_getViewport ptr v)))

    (define/public (map-pixel-to-coords coords v)
      (when (and ptr
                 (vector2i? coords)
                 (view? v))
        (sfRenderTexture_mapPixelToCoords ptr coords v)))

    (define/public (map-coords-to-pixel coords v)
      (when (and ptr
                 (vector2i coords)
                 (view? v))
        (sfRenderTexture_mapCoordsToPixel ptr coords v)))

    (define/public (draw object-clause)
      (when (and ptr
                 (drawable? object-clause))
        (cond [(shape? object-clause)
               (sfRenderTexture_drawShape ptr (send object-clause pointer) #f)]
              [(circle-shape? object-clause)
               (sfRenderTexture_drawCircleShape ptr (send object-clause pointer) #f)]
              [(convex-shape? object-clause)
               (sfRenderTexture_drawConvexShape ptr (send object-clause pointer) #f)]
              [(rectangle-shape? object-clause)
               (sfRenderTexture_drawRectangleShape ptr (send object-clause pointer) #f)]
              [(sprite? object-clause)
               (sfRenderTexture_drawSprite ptr (send object-clause pointer) #f)]
              [(text? object-clause)
               (sfRenderTexture_drawText ptr (send object-clause pointer) #f)]
              [else (printf "type mismatch: expected transformable<%> & drawable<%> implementation, got~a" object-clause)])))

    (define/public (draw* object-clause states)
      (when (and ptr
                 (drawable? object-clause))
        (cond [(shape? object-clause)
               (sfRenderTexture_drawShape ptr (send object-clause pointer) (states->pointer states))]
              [(circle-shape? object-clause)
               (sfRenderTexture_drawCircleShape ptr (send object-clause pointer) (states->pointer states))]
              [(convex-shape? object-clause)
               (sfRenderTexture_drawConvexShape ptr (send object-clause pointer) (states->pointer states))]
              [(rectangle-shape? object-clause)
               (sfRenderTexture_drawRectangleShape ptr (send object-clause pointer) (states->pointer states))]
              [(sprite? object-clause)
               (sfRenderTexture_drawSprite ptr (send object-clause pointer) (states->pointer states))]
              [(text? object-clause)
               (sfRenderTexture_drawText ptr (send object-clause pointer) (states->pointer states))]
              [(vertex-array? object-clause)
               (sfRenderTexture_drawVertexArray ptr (send object-clause pointer) (states->pointer states))]
              [(vertex-buffer? object-clause)
               (sfRenderTexture_drawVertexBuffer ptr (send object-clause pointer) (states->pointer states))]
              [else (printf "type mismatch: expected drawable<%> implementation, got~a" object-clause)])))

    (define/public (draw-primitives arr size primitive states)
      (when ptr
        (sfRenderTexture_drawPrimitives ptr arr size primitive (states->pointer states))))

    (define/public (push-gl-states)
      (when ptr
        (sfRenderTexture_pushGLStates ptr)))

    (define/public (pop-gl-states)
      (when ptr
        (sfRenderTexture_popGLStates ptr)))

    (define/public (reset-gl-states)
      (when ptr
        (sfRenderTexture_resetGLStates ptr)))

    (define/public (get-texture)
      (define out (new texture%))
      (when ptr
        (sfRenderTexture_getTexture ptr)))

    (define/public (set-smooth flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderTexture_setSmooth ptr flag)))

    (define/public (is-smooth?)
      (if ptr
          (sfRenderTexture_isSmooth ptr)
          #f))

    (define/public (set-repeated flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderTexture_setRepeated ptr flag)))

    (define/public (is-repeated?)
      (if ptr
          (sfRenderTexture_isRepeated ptr)
          #f))

    (define/public (generate-mipmap)
      (when ptr
        (sfRenderTexture_generateMipmap ptr)))))

(define (render-texture? object-clause)
  (is-a? object-clause render-texture%))

; ---------------------------------------------------------------------------------------------------------------------------------------------
; RenderWindow.hpp
; ---------------------------------------------------------------------------------------------------------------------------------------------

(define render-window%
  (class object%
    (super-new)
    (field [ptr #f]
           [vmode #f])

    (define/public (make mode title style)
      (unless ptr
        (set! vmode mode)
        (set! ptr (sfRenderWindow_create vmode title style #f))))

    (define/public (make-with-settings mode title style settings)
      (unless ptr
        (set! vmode mode)
        (set! ptr (sfRenderWindow_create vmode title style settings))))

    (define/public (kill)
      (when ptr
        (sfRenderWindow_destroy ptr)
        (set! ptr #f)
        (set! vmode #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfRenderWindow*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (close)
      (when ptr
        (sfRenderWindow_close ptr)))

    (define/public (is-open?)
      (if ptr
          (sfRenderWindow_isOpen ptr)
          #f))

    (define/public (get-settings)
      (if ptr
          (sfRenderWindow_getSettings ptr)
          #f))

    (define/public (poll-event)
      (if ptr
          (sfRenderWindow_pollEvent ptr)
          #f))

    (define/public (wait-event)
      (if ptr
          (sfRenderWindow_waitEvent ptr)
          #f))

    (define/public (get-position)
      (if ptr
          (sfRenderWindow_getPosition ptr)
          #t))

    (define/public (set-position position)
      (when ptr
        (cond [(vector2i? position)
               (sfRenderWindow_setPosition ptr position)]
              [(vector2f? position)
               (sfRenderWindow_setPosition ptr (vector2f->vector2i position))]
              [(pair? position)
               (sfRenderWindow_setPosition ptr (pair->vector2i position))]
              [else (printf "arity mismatch: expected vector2i or pair, got ~a" position)])))

    (define/public (get-size)
      (if ptr
          (sfRenderWindow_getSize ptr)
          #f))

    (define/public (set-size size)
      (when ptr
        (cond [(vector2u? size)
               (sfRenderWindow_setSize ptr size)]
              [(vector2f? size)
               (sfRenderWindow_setSize ptr (vector2f->vector2u size))]
              [(pair? size)
               (sfRenderWindow_setSize ptr (pair->vector2u size))])))

    (define/public (set-title title)
      (when ptr
        (sfRenderWindow_setTitle ptr title)))

    (define/public (set-icon icon)
      (when (and ptr
                 (image? icon))
        (sfRenderWindow_setIcon ptr icon)))

    (define/public (set-visible flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setVisible ptr flag)))

    (define/public (set-vertical-sync-enabled flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setVerticalSyncEnabled ptr flag)))

    (define/public (set-mouse-cursor-visible ptr flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setMouseCursorVisible ptr flag)))

    (define/public (set-mouse-cursor-grabbed ptr flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setMouseCursorGrabbed ptr flag)))

    (define/public (set-mouse-cursor cursor)
      (when ptr
        (sfRenderWindow_setMouseCursor ptr cursor)))

    (define/public (set-key-repeat-enabled flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setKeyRepeatEnabled ptr flag)))

    (define/public (set-framerate-limit limit)
      (when ptr
        (sfRenderWindow_setFramerateLimit ptr (abs (inexact->exact (floor limit))))))

    (define/public (set-joystick-threshold thresh)
      (when ptr
        (sfRenderWindow_setJoystickThreshold ptr (exact->inexact thresh))))

    (define/public (set-active flag)
      (when (and ptr
                 (boolean? flag))
        (sfRenderWindow_setActive ptr flag)))

    (define/public (request-focus)
      (when ptr
        (sfRenderWindow_requestFocus ptr)))

    (define/public (has-focus?)
      (if ptr
          (sfRenderWindow_hasFocus ptr)
          #f))

    (define/public (display)
      (when ptr
        (sfRenderWindow_display ptr)))

    (define/public (clear)
      (when ptr
        (sfRenderWindow_clear ptr sfBlack)))

    (define/public (clear* clear-color)
      (when (and ptr
                 (color? clear-color))
        (sfRenderWindow_clear ptr clear-color)))

    (define/public (set-view v)
      (when (and ptr
                 (view? v))
        (sfRenderWindow_setView ptr (send v pointer))))

    (define/public (get-view)
      (define out (new view%))
      (when ptr
        (send out set-pointer (sfRenderWindow_getView ptr)))
      out)

    (define/public (get-default-view)
      (define out (new view%))
      (when ptr
        (send out set-pointer (sfRenderWindow_getDefaultView ptr)))
      out)

    (define/public (get-viewport)
      (if ptr
          (sfRenderWindow_getViewport ptr)
          #f))

    (define/public (map-pixel-to-coords point view)
      (when ptr
        (cond [(vector2i? point)
               (sfRenderWindow_mapPixelToCoords ptr point (send view pointer))]
              [(vector2f? point)
               (sfRenderWindow_mapPixelToCoords ptr (vector2f->vector2i ptr) (send view pointer))]
              [(pair? point)
               (sfRenderWindow_mapPixelToCoords ptr (pair->vector2i ptr) (send view pointer))]
              [else (printf "type mismatch: expected vector2i or pair, got ~a" point)])))

    (define/public (map-coords-to-pixel ptr point view)
      (when ptr
        (cond [(vector2i? point)
               (sfRenderWindow_mapCoordsToPixel ptr point (send view pointer))]
              [(vector2f? point)
               (sfRenderWindow_mapCoordsToPixel ptr (vector2f->vector2i ptr) (send view pointer))]
              [(pair? point)
               (sfRenderWindow_mapCoordsToPixel ptr (pair->vector2i ptr) (send view pointer))]
              [else (printf "type mismatch: expected vector2i or pair, got ~a" point)])))

    (define/public (draw object-clause)
      (define states (make-render-states))
      (when (and ptr
                 (drawable? object-clause))
        (send object-clause draw ptr (states->pointer states))))

    (define/public (push-gl-states)
      (when ptr
        (sfRenderWindow_pushGLStates ptr)))

    (define/public (pop-gl-states)
      (when ptr
        (sfRenderWindow_popGLStates ptr)))

    (define/public (reset-gl-states)
      (when ptr
        (sfRenderWindow_resetGLStates ptr)))

    (define/public (capture)
      (when ptr
        (sfRenderWindow_capture ptr)))))

(define (render-window? object-clause)
  (is-a? object-clause render-window%))

; --------------------------------------------------------------------------------------------------------------------
; Shader.hpp
; --------------------------------------------------------------------------------------------------------------------
