#lang racket
(require csfml)
(require (file "system.rkt"))

(provide (all-defined-out))

; -----------------------------------------------
; Event.hpp
; -----------------------------------------------
(define event-type sfEvent-type)
(define event-joystick-button sfEvent-joystickButton)
(define event-joystick-move sfEvent-joystickMove)
(define event-key sfEvent-key)
(define event-mouse-button sfEvent-mouseButton)
(define event-mouse-move sfEvent-mouseMove)
(define event-sensor sfEvent-sensor)
(define event-size sfEvent-size)
(define event-text sfEvent-text)
(define event-touch sfEvent-touch)

; joystick button
(define (joystick-button-id e)
  (sfJoystickButtonEvent-joystickId (event-joystick-button e)))

(define (joystick-button-code e)
  (sfJoystickButtonEvent-button (event-joystick-button e)))

; joystick move
(define (joystick-move-id e)
  (sfJoystickMoveEvent-joystickId (event-joystick-move e)))

(define (joystick-move-axis e)
  (sfJoystickMoveEvent-axis (event-joystick-move e)))

(define (joystick-move-position e)
  (sfJoystickMoveEvent-position (event-joystick-move e)))

; key 
(define (event-key-code e)
  (sfKeyEvent-code (event-key e)))

(define (event-key-alt? e)
  (sfKeyEvent-alt (event-key e)))

(define (event-key-control e)
  (sfKeyEvent-control (event-key e)))

(define (event-key-shift e)
  (sfKeyEvent-shift (event-key e)))

(define (event-key-system e)
  (sfKeyEvent-system (event-key e)))


; mouse button
(define (event-mouse-button-code e)
  (sfMouseButtonEvent-button (event-mouse-button e)))

(define (event-mouse-button-x e)
  (sfMouseButtonEvent-x (event-mouse-button e)))

(define (event-mouse-button-y e)
  (sfMouseButtonEvent-y (event-mouse-button e)))

; mouse move
(define (event-mouse-move-x e)
  (sfMouseMoveEvent-x (event-mouse-move e)))

(define (event-mouse-move-y e)
  (sfMouseMoveEvent-y (event-mouse-move e)))


; sensor
(define (event-sensor-type e)
  (sfSensorEvent-type (event-sensor e)))

(define (event-sensor-x e)
  (sfSensorEvent-x (event-sensor e)))

(define (event-sensor-y e)
  (sfSensorEvent-y (event-sensor e)))

(define (event-sensor-z e)
  (sfSensorEvent-z (event-sensor e)))

; size
(define (event-size-width e)
  (sfSizeEvent-width (event-size e)))

(define (event-size-height e)
  (sfSizeEvent-height (event-size e)))

; text
(define (event-text-unicode e)
  (sfTextEvent-unicode (event-text e)))

; touch

(define (event-touch-finger e)
  (sfTouchEvent-finger (event-touch e)))

(define (event-touch-x e)
  (sfTouchEvent-x (event-touch e)))

(define (event-touch-y e)
  (sfTouchEvent-y (event-touch e)))

(define joystick-id make-sfJoystickIdentification)

; -------------------------------------------------------------
; VideoMode.hpp
; -------------------------------------------------------------

(define (video-mode w h b)
  (make-sfVideoMode (abs (inexact->exact (truncate w)))
                    (abs (inexact->exact (truncate h)))
                    (abs (inexact->exact (truncate b)))))

(define video-mode? sfVideoMode?)

(define is-valid-video-mode? sfVideoMode_isValid)
(define is-valid-mode? sfVideoMode_isValid)

(define get-desktop-mode sfVideoMode_getDesktopMode)
(define desktop-mode sfVideoMode_getDesktopMode)

(define get-fullscreen-mode sfVideoMode_getFullscreenModes)
(define fullscreen-mode sfVideoMode_getFullscreenModes)

; -----------------------------------------------------------
; Clipboard.hpp
; -----------------------------------------------------------

(define get-clipboard-string sfClipboard_getString)
(define set-clipboard-string sfClipboard_setString)


; ----------------------------------------------------------
; Context.hpp
; ----------------------------------------------------------

(define context%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (when (not ptr)
        (set! ptr (sfContext_create))))

    (define/public (kill)
      (when ptr
        (sfContext_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfContext*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (is-valid?)
      (send this valid?))

    (define/public (set-active flag)
      (when ptr
        (sfContext_setActive ptr flag)))

    (define/public (get-active-id)
      (if ptr
          (sfContext_getActiveContextId)
          #f))))

(define get-active-context-id sfContext_getActiveContextId)

(define (context? context-clause)
  (is-a? context-clause context%))

; ----------------------------------------------------------
; Cursor.hpp
; ----------------------------------------------------------

(define cursor%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make-from-pixels p a b)
      (when (not ptr)
        (set! ptr (sfCursor_createFromPixels p a b))))

    (define/public (make-from-system t)
      (when (not ptr)
        (set! ptr (sfCursor_createFromSystem t))))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfCursor*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (is-valid?)
      (send this valid?))
    
    (define/public (kill)
      (when ptr
        (sfCursor_destroy ptr)))))

(define (cursor? cursor-clause)
  (is-a? cursor-clause cursor%))

; -------------------------------------------------------
; Keyboard.hpp
; -------------------------------------------------------

(define key-pressed? sfKeyboard_isKeyPressed)
(define set-virtual-keyboard-visible sfKeyboard_setVirtualKeyboardVisible)

; ------------------------------------------------------
; Mouse.hpp
; ------------------------------------------------------

(define mouse-button-pressed? sfMouse_isButtonPressed)
(define mouse-position-set! sfMouse_setPosition)
(define mouse-position sfMouse_getPosition)

; -----------------------------------------------------
; Sensor.hpp
; -----------------------------------------------------

(define sensor-available? sfSensor_isAvailable)
(define sensor-enabled sfSensor_setEnabled)
(define sensor-value sfSensor_getValue)

; ----------------------------------------------------
; Touch.hpp
; ----------------------------------------------------

(define touch-down? sfTouch_isDown)
(define touch-position sfTouch_getPosition)

; ----------------------------------------------------
; Window.hpp
; ----------------------------------------------------

(define window%
  (class object%
    (super-new)

    (field [ptr #f]
           [vmode #f]
           [ctx #f])

    (define/public (make mode title style)
      (unless ptr
        (set! vmode mode)
        (set! ptr (sfWindow_create vmode title style ctx))))

    (define/public (make* mode title style context-settings)
      (unless ptr
        (set! vmode mode)
        (set! ctx context-settings)
        (set! ptr (sfWindow_create vmode title style ctx))))

    (define/public (kill)
      (when ptr
        (sfWindow_destroy ptr)
        (set! ptr #f)
        (set! vmode #f)
        (set! ctx #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfWindow*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (is-valid?)
      (send this valid?))

    (define/public (close)
      (when ptr
        (sfWindow_close ptr)))

    (define/public (open?)
      (if ptr
          (sfWindow_isOpen ptr)
          #f))

    (define/public (is-open?)
      (send this open?))

    (define/public (get-settings)
      (if ptr
          (sfWindow_getSettings ptr)
          #f))

    (define/public (poll-event)
      (if ptr
          (sfWindow_pollEvent ptr)
          #f))

    (define/public (wait-event)
      (if ptr
          (sfWindow_waitEvent ptr)
          #f))

    (define/public (get-position)
      (if ptr
          (sfWindow_getPosition ptr)
          #f))

    (define/public (set-position v2)
      (if (vector2i? v2)
          (when ptr
            (sfWindow_setPosition ptr v2))
          (printf "expected _sfVector2i, got ~a" v2)))

    (define/public (set-position* x y)
      (send this set-position (vector2i x y)))
    
    (define/public (get-size)
      (if ptr
          (sfWindow_getSize ptr)
          #f))

    (define/public (set-size v2)
      (if (sfVector2u? v2)
          (when ptr
            (sfWindow_setSize ptr v2))
          (printf "expected _sfVector2u, got ~a" v2)))

    (define/public (set-size* x y)
      (send this set-size (vector2u x y)))

    (define/public (set-title title)
      (when ptr
        (sfWindow_setTitle ptr)))

    (define/public (set-visible flag)
      (when ptr
        (sfWindow_setVisible ptr flag)))

    (define/public (set-vertical-sync-enabled flag)
      (when ptr
        (sfWindow_setVerticalSyncEnabled)))

    (define/public (set-mouse-cursor-visible flag)
      (when ptr
        (sfWindow_setMouseCursorVisible ptr flag)))

    (define/public (set-mouse-cursor-grabbed flag)
      (when ptr
        (sfWindow_setMouseCursorGrabbed ptr flag)))

    (define/public (set-mouse-cursor mouse-cursor)
      (when ptr
        (if (cursor? mouse-cursor)
            (sfWindow_setMouseCursor ptr (send mouse-cursor pointer))
            (printf "(send ~a set-mouse-cursor ~a) got ~a" this cursor% mouse-cursor))))

    (define/public (set-key-repeat-enabled flag)
      (when ptr
        (sfWindow_setKeyRepeatEnabled ptr flag)))

    (define/public (set-framerate-limit limit)
      (when ptr
        (sfWindow_setFramerateLimit ptr (abs (inexact->exact (floor limit))))))

    (define/public (set-joystick-threshhold thresh)
      (when ptr
        (sfWindow_setJoystickThreshold ptr (exact->inexact thresh))))

    (define/public (set-active flag)
      (when ptr
        (sfWindow_setActive ptr flag)))

    (define/public (has-focus?)
      (if ptr
          (sfWindow_hasFocus ptr)
          #f))

    (define/public (display)
      (when ptr
        (sfWindow_display ptr)))))

(define (window? window-clause)
  (is-a? window-clause window%))
                 