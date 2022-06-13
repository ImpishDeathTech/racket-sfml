#lang racket

#|
A example of to create a very simple right angled triangle
using a vertex array
|#

(require sfml)

(define window (new render-window%))
(define right-triangle (new vertex-array%))

; create the window
(send* window
  (make (video-mode 800 600 32) "SFML Works! >:3" '(sfDefaultStyle))
  (clear)
  (display)
  (set-visible #f))

#|
create the triangle. each point is set to a different color
to show you where they are
|#
(send* right-triangle
  (make)
  (set-primitive-type 'sfTriangles)
  ; yellow corner
  (append
   (vertex (vector2f 10 10)
           (color 255 255 0)
           (vector2f 0 0)))
  ; magenta corner
  (append
   (vertex (vector2f 200 10)
           (color 255 0 255)
           (vector2f 0 0)))
  ; cyan corner
  (append
   (vertex (vector2f 200 200)
           (color 0 255 255)
           (vector2f 0 0))))

; poll the windows events
(define (poll-events)
  (let ([e (send window poll-event)])
    (when e
      (case (event-type e)
        ('sfEvtClosed
         (send window close))
        ('sfEvtKeyPressed
         (case (event-key-code e)
           ('sfKeyEscape
            (send window close)))))
      (poll-events))))

; render the triangle
(define (render)
  (send* window
    (clear)
    (draw right-triangle)
    (display)))

; run the loop
(define (game-loop)
  (when (send window is-open?)
    (poll-events)
    (render)
    (game-loop)))

; make the window visible if you hid it during setup
(send window set-visible #t)

; run the program
(with-handlers
    ([exn?
      (λ (exn)
        (println exn)
        (send window close))])
  (game-loop))

; kill the objects
(send window kill)
(send right-triangle kill)
