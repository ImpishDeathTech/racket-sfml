#lang racket
(require csfml)
(require (file "system.rkt"))

(provide (all-defined-out))

; ---------------------------------------------------------------------------------------------------
; Listener.hpp
; ---------------------------------------------------------------------------------------------------

(define (listener-global-volume-set! vol)
  (sfListener_setGlobalVolume (exact->inexact vol)))

(define listener-global-volume sfListener_getGlobalVolume)

(define (listener-directon-set! v3)
  (cond [(vector3? v3)
         (sfListener_setDirection v3)]
        [(and (list? v3)
              (= (length v3) 3))
         (sfListener_setDirection (vector3 (car v3)
                                           (cadr v3)
                                           (caddr v3)))]
        [else (printf "type mismatch: expected vector3 or list (length 3), got ~a" v3)]))

(define listener-directon sfListener_getDirection)

(define (listener-up-vector-set! v3)
  (cond [(vector3? v3)
         (sfListener_setUpVector v3)]
        [(and (list? v3)
              (= (length v3) 3))
         (sfListener_setUpVector (vector3 (car v3)
                                          (cadr v3)
                                          (caddr v3)))]
        [else (printf "type mismatch: expected vector3 or list (length 3), got ~a" v3)]))

(define listener-up-vector sfListener_getUpVector)

; ------------------------------------------------------------------------------------------------
; Music.hpp
; ------------------------------------------------------------------------------------------------

(define music%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make-from-file path)
      (unless ptr
        (set! ptr (sfMusic_createFromFile path))))


    (define/public (make-from-memory data size)
      (unless ptr
        (set! ptr (sfMusic_createFromMemory data size))))

    (define/public (make-from-stream istream)
      (unless ptr
        (set! ptr (sfMusic_createFromStream istream))))

    (define/public (kill)
      (when ptr
        (sfMusic_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr p))
    
    (define/public (set-loop flag)
      (when ptr
        (if (boolean? flag)
            (sfMusic_setLoop ptr flag)
            (sfMusic_setLoop ptr #f))))

    (define/public (get-loop)
      (when ptr
        (sfMusic_getLoop ptr)))

    (define/public (get-duration)
      (when ptr
        (sfMusic_getDuration ptr)))

    (define/public (get-loop-points)
      (when ptr
        (sfMusic_getLoopPoints ptr)))

    (define/public (set-loop-points span)
      (when ptr
        (sfMusic_setLoopPoints ptr span)))

    (define/public (play)
      (when ptr
        (sfMusic_play ptr)))

    (define/public (stop)
      (when ptr
        (sfMusic_stop ptr)))

    (define/public (pause)
      (when ptr
        (sfMusic_pause ptr)))

    (define/public (get-channel-count)
      (when ptr
        (sfMusic_getChannelCount ptr)))

    (define/public (get-sample-rate)
      (when ptr
        (sfMusic_getSampleRate ptr)))

    (define/public (get-status)
      (when ptr
        (sfMusic_getStatus ptr)))

    (define/public (get-playing-offset)
      (when ptr
        (sfMusic_getPlayingOffset ptr)))

    (define/public (set-pitch pitch)
      (when ptr
        (sfMusic_setPitch ptr (exact->inexact pitch))))

    (define/public (set-volume vol)
      (when ptr
        (sfMusic_setVolume ptr (exact->inexact vol))))

   (define/public (set-position v3)
     (when ptr
       (cond [(vector3? v3)
              (sfMusic_setPosition ptr v3)]
             [(and (list? v3)
                   (= (length v3) 3))
              (sfMusic_setPosition ptr (vector3 (car v3)
                                                (cadr v3)
                                                (caddr v3)))]
             [else (printf "arity mismatch: expected vector3 or list (length 3), got ~a" v3)])))

    (define/public (set-relative-to-listener flag)
      (when ptr
        (if (boolean? flag)
            (sfMusic_setRelativeToListener ptr flag)
            (sfMusic_setRelativeToListener ptr #f))))

    (define/public (set-min-distance dist)
      (when ptr
        (sfMusic_setMinDistance ptr (exact->inexact dist))))

    (define/public (set-attenuation att)
      (when ptr
        (sfMusic_setAttenuation ptr (exact->inexact att))))

    (define/public (set-playing-offset offset)
      (when ptr 
        (if (time-struct? offset)
            (sfMusic_setPlayingOffset ptr offset)
            (sfMusic_setPlayingOffset ptr (seconds 0)))))

    (define/public (get-pitch)
      (when ptr
        (sfMusic_getPitch ptr)))

    (define/public (get-volume)
      (when ptr
        (sfMusic_getVolume ptr)))

    (define/public (get-position)
      (when ptr
        (sfMusic_getPosition ptr)))

    (define/public (relative-to-listener?)
      (when ptr
        (sfMusic_isRelativeToListener ptr)))

    (define/public (get-min-distance)
      (when ptr
        (sfMusic_getMinDistance ptr)))

    (define/public (get-attenuation)
      (when ptr
        (sfMusic_getAttenuation ptr)))))

(define (music? object-clause)
  (is-a? object-clause music%))

; --------------------------------------------------------------------------------------------------------------
; SoundBuffer.hpp
; --------------------------------------------------------------------------------------------------------------

(define sound-buffer%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make-from-file path)
      (unless ptr
        (set! ptr (sfSoundBuffer_createFromFile path))))

    (define/public (make-from-memory data size)
      (unless ptr
        (set! ptr (sfSoundBuffer_createFromMemory data size))))

    (define/public (make-from-stream istream)
      (unless ptr
        (set! ptr (sfSoundBuffer_createFromStream istream))))

    (define/public (make-from-samples samples sample-count channel-count sample-rate)
      (unless ptr
        (set! ptr (sfSoundBuffer_createFromSamples samples
                                                   sample-count
                                                   (abs (round channel-count))
                                                   (abs (round sample-rate))))))

    (define/public (kill)
      (when ptr
        (sfSoundBuffer_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr p))
    
    (define/public (copy)
      (when ptr
        (sfSoundBuffer_copy ptr)))

    (define/public (save-to-file path)
      (when ptr
        (sfSoundBuffer_saveToFile ptr path)))

    (define/public (get-samples)
      (when ptr
        (sfSoundBuffer_getSamples ptr)))

    (define/public (get-sample-count)
      (when ptr
        (sfSoundBuffer_getSampleCount ptr)))

    (define/public (get-sample-rate)
      (when ptr
        (sfSoundBuffer_getSampleRate ptr)))

    (define/public (get-channel-count)
      (when ptr
        (sfSoundBuffer_getChannelCount ptr)))

    (define/public (get-duration)
      (when ptr
        (sfSoundBuffer_getDuration ptr)))))

(define (sound-buffer? object-clause)
  (is-a? object-clause sound-buffer%))

; ------------------------------------------------------------------------------------------------------------------------
; SoundBufferRecorder.hpp
; ------------------------------------------------------------------------------------------------------------------------

(define sound-buffer-recorder%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfSoundBufferRecorder_create))))

    (define/public (kill)
      (when ptr
        (sfSoundBufferRecorder_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr #f))

    (define/public (start sample-rate)
      (when ptr
        (sfSoundBufferRecorder_start ptr sample-rate)))

    (define/public (stop)
      (when ptr
        (sfSoundBufferRecorder_stop ptr)))

    (define/public (get-sample-rate)
      (when ptr
        (sfSoundBufferRecorder_getSampleRate ptr)))

    (define/public (get-buffer)
      (when ptr
        (sfSoundBufferRecorder_getBuffer ptr)))

    (define/public (set-device device-string)
      (when ptr
        (sfSoundBufferRecorder_setDevice ptr device-string)))

    (define/public (get-device)
      (when ptr
        (sfSoundBufferRecorder_getDevice ptr)))))

(define (sound-buffer-recorder? object-clause)
  (is-a? object-clause sound-buffer-recorder%))

; ------------------------------------------------------------------------------------------------------------------------
; Sound.hpp
; ------------------------------------------------------------------------------------------------------------------------

(define sound%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfSound_create))))

    (define/public (kill)
      (when ptr
        (sfSound_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr p))

    (define/public (copy)
      (when ptr
        (sfSound_copy ptr)))

    (define/public (play)
      (when ptr
        (sfSound_play ptr)))


    (define/public (pause)
      (when ptr
        (sfSound_pause ptr)))

    (define/public (stop)
      (when ptr
        (sfSound_stop ptr)))

    (define/public (set-buffer buffer)
      (when (and ptr
                 (sound-buffer? buffer))
        (sfSound_setBuffer ptr (send buffer pointer))))

    (define/public (get-buffer)
      (when ptr
        (define out (new sound-buffer%))
        (send out set-pointer (sfSound_getBuffer ptr))))

    (define/public (set-loop flag)
      (when ptr
        (if (boolean? flag)
            (sfSound_setLoop ptr flag)
            (sfSound_setLoop ptr #f))))

    (define/public (get-loop)
      (when ptr
        (sfSound_getLoop ptr)))

    (define/public (get-status)
      (when ptr
        (sfSound_getStatus ptr)))

    (define/public (set-pitch pitch)
      (when ptr
        (sfSound_setPitch ptr (exact->inexact pitch))))

    (define/public (set-volume vol)
      (when ptr
        (sfSound_setVolume ptr (exact->inexact vol))))

    (define/public (set-position v3)
      (when ptr
        (cond [(vector3? v3)
               (sfSound_setPosition ptr v3)]
              [(and (list? v3)
                    (= (length v3) 3))
               (sfSound_setPosition ptr (vector3 (car v3)
                                                 (cadr v3)
                                                 (caddr v3)))]
              [else (printf "arity mismatch: expected vector3 or list (length 3), got ~a" v3)])))

    (define/public (set-relative-to-listener flag)
      (when ptr
        (if (boolean? flag)
            (sfSound_setRelativeToListener ptr flag)
            (sfSound_setRelativeToListener ptr #f))))

    (define/public (set-min-distance dist)
      (when ptr
        (sfSound_setMinDistance ptr (exact->inexact dist))))

    (define/public (set-attenuation att)
      (when ptr
        (sfSound_setAttenuation ptr (exact->inexact att))))

    (define/public (set-playing-offset offset)
      (when ptr
        (sfSound_setPlayingOffset ptr offset)))

    (define/public (get-pitch)
      (when ptr
        (sfSound_getPitch ptr)))

    (define/public (get-volume)
      (when ptr
        (sfSound_getVolume ptr)))

    (define/public (relative-to-listener?)
      (when ptr
        (sfSound_isRelativeToListener ptr)))

    (define/public (get-min-distance)
      (when ptr
        (sfSound_getMinDistance ptr)))

    (define/public (get-attenuation)
      (when ptr
        (sfSound_getAttenuation ptr)))

    (define/public (get-playing-offset)
      (when ptr
        (sfSound_getPlayingOffset ptr)))))

(define (sound? object-clause)
  (is-a? object-clause sound%))

; ---------------------------------------------------------------------------------------------
; SoundRecorder.hpp
; ---------------------------------------------------------------------------------------------

(define sound-recorder%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make on-start on-process on-stop user-data)
      (unless ptr
        (set! ptr (sfSoundRecorder_create on-start on-process on-stop user-data))))

    (define/public (kill)
      (when ptr
        (sfSoundBuffer_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr p))

    (define/public (start sample-rate)
      (when ptr
        (sfSoundRecorder_start ptr (abs (round sample-rate)))))

    (define/public (stop)
      (when ptr
        (sfSoundRecorder_stop ptr)))
    
    (define/public (get-sample-rate)
      (when ptr
        (sfSoundRecorder_getSampleRate ptr)))


    (define/public (available?)
      (if ptr
          (sfSoundRecorder_isAvailable ptr)
          #f))

    (define/public (set-processing-interval interval)
      (when ptr
        (if (time-struct? interval)
            (sfSoundRecorder_setProcessingInterval ptr interval)
            (sfSoundRecorder_setProcessingInterval ptr (seconds 0)))))

    (define/public (get-available-devices)
      (when ptr
        (sfSoundRecorder_getAvailableDevices ptr)))

    (define/public (get-default-device)
      (when ptr
        (sfSoundRecorder_getDefaultDevice ptr)))

    (define/public (set-device device-string)
      (when ptr
        (sfSoundRecorder_setDevice ptr device-string)))

    (define/public (get-device)
      (when ptr
        (sfSoundRecorder_getDevice ptr)))

    (define/public (set-channel-count channel-count)
      (when ptr
        (sfSoundRecorder_setChannelCount ptr (abs (round channel-count)))))

    (define/public (get-channel-count)
      (when ptr
        (sfSoundRecorder_getChannelCount ptr)))))

(define (sound-recorder? object-clause)
  (is-a? object-clause sound-recorder%))

; --------------------------------------------------------------------------------------------------------------
; SoundStream.hpp
; --------------------------------------------------------------------------------------------------------------

(define sound-stream%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make on-get-data on-seek channel-count sample-rate user-data)
      (unless ptr
        (set! ptr (sfSoundStream_create on-get-data on-seek channel-count sample-rate user-data))))

    (define/public (kill)
      (when ptr
        (sfSoundStream_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (send this kill)
      (set! ptr p))

    (define/public (play)
      (when ptr
        (sfSoundStream_play ptr)))

    (define/public (pause)
      (when ptr
        (sfSoundStream_pause ptr)))

    (define/public (stop)
      (when ptr
        (sfSoundStream_stop ptr)))

    (define/public (get-status)
      (when ptr
        (sfSoundStream_getStatus ptr)))

    (define/public (get-channel-count)
      (when ptr
        (sfSoundStream_getChannelCount ptr)))

    (define/public (get-sample-rate)
      (when ptr
        (sfSoundStream_getSampleRate ptr)))

    (define/public (set-pitch pitch)
      (when ptr
        (sfSoundStream_setPitch ptr (exact->inexact pitch))))

    (define/public (set-volume vol)
      (when ptr
        (sfSoundStream_setVolume ptr (exact->inexact vol))))

    (define/public (set-position position)
      (when ptr
        (cond [(vector3? position)
               (sfSoundStream_setPosition ptr position)]
              [(and (list? position)
                    (= (length position) 3))
               (sfSoundStream_setPosition ptr (vector3 (car position)
                                                       (cadr position)
                                                       (caddr position)))]
              [else (printf "arity mismatch: expected vector3 or list (length 3), got ~a" position)])))

    (define/public (set-relative-to-listener flag)
      (when ptr
        (if (boolean? flag)
            (sfSoundStream_setRelativeToListener ptr flag)
            (sfSoundStream_setRelativeToListener ptr #f))))

    (define/public (set-min-distance dist)
      (when ptr
        (sfSoundStream_setMinDistance ptr (exact->inexact dist))))

    (define/public (set-attenuation att)
      (when ptr
        (sfSoundStream_setAttenuation ptr (exact->inexact att))))

    (define/public (set-playing-offset offset)
      (when ptr
        (sfSoundStream_setPlayingOffset ptr offset)))

    (define/public (set-loop flag)
      (when ptr
        (if (boolean? flag)
            (sfSoundStream_setLoop ptr flag)
            (sfSoundStream_setLoop ptr #f))))

    (define/public (get-pitch)
      (when ptr
        (sfSoundStream_getPitch ptr)))

    (define/public (get-volume)
      (when ptr
        (sfSoundStream_getVolume ptr)))

    (define/public (relative-to-listener?)
      (when ptr
        (sfSoundStream_isRelativeToListener ptr)))

    (define/public (get-min-distance)
      (when ptr
        (sfSoundStream_getMinDistance ptr)))

    (define/public (get-loop)
      (when ptr
        (sfSoundStream_getLoop ptr)))

    (define/public (get-playing-offset)
      (when ptr
        (sfSoundStream_getPlayingOffset ptr)))))

(define (sound-stream? object-clause)
  (is-a? object-clause sound-stream%))