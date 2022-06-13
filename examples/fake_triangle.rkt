#lang racket

(require sfml)

#|
this program is a clone of ex_3.rkt from
the racket-csfml examples
but it turns the circle into a triangle with a little trick >;3
|#

; define your objects like so
(define window (new render-window%))
(define fake-triangle (new circle-shape%))

; make the main window
(send* window
  (make (video-mode 800 600 32) "SFML Fake Triangle Works! ^,..,^" '(sfDefaultStyle))
  (set-visible #f))

; make the fake triangle
(send* fake-triangle
  (make)
  (set-radius 20)
  (set-point-count 3) 
  (set-fill-color (color 0 0 0 0))
  (set-outline-color (color 255 0 255))
  (set-outline-thickness 3)
  (set-origin (vector2f 20 20)))

; convert integer pair to vector2 in view space
(define (map-pixel x y)
  (let ([pos (vector2i x y)]
        [view (send window get-view)])
    (send window map-pixel-to-coords pos view)))

; poll your window's events
(define (poll-events)
  (let ([e (send window poll-event)])
    (when e
      (case (event-type e)
        ('sfEvtClosed
         (send window close))
        ('sfEvtMouseMoved
         (let* ([pos (map-pixel (event-mouse-move-x e)
                                (event-mouse-move-y e))])
                (send circle set-position pos)))
        ('sfEvtKeyPressed
         (case (event-key-code e)
           ('sfKeyEscape
            (send window close)))))
      (poll-events))))

; render your fake triangle
(define (render)
  (send* window
    (clear)
    (draw fake-triangle)
    (display)))

(define (game-loop)
  (poll-events)
  (render)
  (when (send window is-open?)
    (game-loop)))

(send* window
  (set-visible #t)
  (clear)
  (display))

(with-handlers
    ([exn?
     (λ (exn)
       (displayln exn)
       (send window close))])
     (game-loop))

#|
always kill your sfml objects when finished with them to free the memory
they are still csfml pointers under the hood, after all
I like to define an object manager class with a kill-all function
to do this in one command and keep my code cleaner, usually
|#

(send window kill)
(send fake-triangle kill)
