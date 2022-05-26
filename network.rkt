#lang racket
(require csfml)
(require (file "system.rkt"))

(provide (all-defined-out))

; ----------------------------------------------------------------------------
; IpAddress.hpp
; ----------------------------------------------------------------------------

(define ip-address? sfIpAddress?)
(define ip-address make-sfIpAddress)
(define string->ip-address sfIpAddress_fromString)
(define bytes->ip-address sfIpAddress_fromBytes)
(define integer->ip-address sfIpAddress_fromInteger)
(define ip-address->string sfIpAddress_toString)
(define ip-address->integer sfIpAddress_toInteger)
(define ip-address:local-address sfIpAddress_getLocalAddress)
(define ip-address:public-address sfIpAddress_getPublicAddress)

; ----------------------------------------------------------------------------
; Ftp.hpp
; ----------------------------------------------------------------------------

(define ftp-response<%>
  (interface () pointer set-pointer kill ok? get-status get-message))

(define (implements-ftp-response? object-clause)
  (is-a? object-clause ftp-response<%>))

(define ftp-listing-response%
  (class* object%
    (ftp-response<%>)
    (super-new)
    (field [ptr #f])

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfFtpListingResponse*? p)
        (set! ptr p)))

    (define/public (kill)
      (when ptr
        (sfFtpListingResponse_destroy ptr)
        (set! ptr #f)))

    (define/public (ok?)
      (when ptr
        (sfFtpListingResponse_isOk ptr)))

    (define/public (get-status)
      (when ptr
        (sfFtpListingResponse_getStatus ptr)))

    (define/public (get-message)
      (when ptr
        (sfFtpListingResponse_getMessage ptr)))

    (define/public (get-count)
      (when ptr
        (sfFtpListingResponse_getCount ptr)))

    (define/public (get-name index)
      (when ptr
        (sfFtpListingResponse_getName ptr (inexact->exact (round index)))))))

(define (ftp-listing-response? object-clause)
  (is-a? object-clause ftp-listing-response%))

(define ftp-directory-response%
  (class* object%
    (ftp-response<%>)
    (super-new)
    (field [ptr #f])

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfFtpDirectoryResponse*? p)
        (set! ptr p)))

    (define/public (kill)
      (when ptr
        (sfFtpDirectoryResponse_destroy ptr)
        (set! ptr #f)))

    (define/public (ok?)
      (when ptr
        (sfFtpDirectoryResponse_isOk ptr)))

    (define/public (get-status)
      (when ptr
        (sfFtpDirectoryResponse_getStatus ptr)))

    (define/public (get-message)
      (when ptr
        (sfFtpDirectoryResponse_getMessage ptr)))

    (define/public (get-directory)
      (when ptr
        (sfFtpDirectoryResponse_getDirectory ptr)))))

(define (ftp-directory-response? object-clause)
  (is-a? object-clause ftp-directory-response%))

(define ftp-response%
  (class* object%
    (ftp-response<%>)
    (super-new)
    (field [ptr #f])

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfFtpResponse*? p)
        (set! ptr p)))

    (define/public (kill)
      (when ptr
        (sfFtpResponse_destroy ptr)
        (set! ptr #f)))
    
    (define/public (ok?)
      (when ptr
        (sfFtpResponse_isOk ptr)))

    (define/public (get-status)
      (when ptr
        (sfFtpResponse_getStatus ptr)))

    (define/public (get-message)
      (when ptr
        (sfFtpResponse_getMessage ptr)))))

(define (ftp-response? object-clause)
  (is-a? object-clause ftp-response%))

(define (ftp-response-set? object-clause)
  (if (implements-ftp-response? object-clause)
      (if (send object-clause pointer)
          #t
          #f)
      #f))

(define ftp%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfFtp_create))))

    (define/public (kill)
      (when ptr
        (sfFtp_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfFtp*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (connect server port timeout)
      (define out (new ftp-response%))
      (when (and ptr
                 (ip-address? server)
                 (exact-nonnegative-integer? port)
                 (time-struct? timeout))
        (send out
              set-pointer
              (sfFtp_connect ptr
                             (send server pointer)
                             port
                             (send timeout pointer))))
      out)

    (define/public (login-anonymous)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_loginAnonymous ptr)))
      out)

    (define/public (login name password)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_login ptr name password)))
      out)

    (define/public (disconnect)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_disconnect ptr)))
      out)

    (define/public (keep-alive)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_keepAlive ptr)))
      out)

    (define/public (get-working-directory)
      (define out (new ftp-directory-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_getWorkingDirectory ptr)))
      out)

    (define/public (get-directory-listing directory)
      (define out (new ftp-listing-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_getDirectoryListing ptr directory)))
      out)

    (define/public (change-directory directory)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_changeDirectory ptr directory)))
      out)

    (define/public (parent-directory)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_parentDirectory ptr)))
      out)

    (define/public (make-directory name)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_createDirectory ptr name)))
      out)

    (define/public (mkdir name)
      (send this make-directory name))

    (define/public (create-directory name)
      (send this make-directory name))
    
    (define/public (remove-directory name)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_deleteDirectory ptr name)))
      out)

    (define/public (delete-directory name)
      (send this remove-directory name))

    (define/public (rename-file file-name new-name)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_renameFile ptr file-name new-name)))
      out)

    (define/public (remove-file name)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_deleteFile ptr name)))
      out)

    (define/public (delete-file name)
      (send this remove-file name))

    (define/public (rm sym name)
      (cond [(eq? sym 'file)
             (send this remove-file name)]
            [(eq? sym 'dir)
             (send this remove-directory name)]
            [else
             (printf "arity mismatch: expected 'file or 'dir, got ~a" sym)
             #f]))

    (define/public (download remote-file local-path mode)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_download ptr remote-file local-path mode))))

    (define/public (upload local-file remote-path mode append?)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_upload ptr local-file remote-path mode append?)))
      out)

    (define/public (send-command command parameter)
      (define out (new ftp-response%))
      (when ptr
        (send out
              set-pointer
              (sfFtp_sendCommand ptr command parameter))))))

(define (ftp? object-clause)
  (is-a? object-clause ftp%))

; ----------------------------------------------------------------------------------------
; Http.hpp    
; ----------------------------------------------------------------------------------------

(define http-request%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfHttpRequest_create))))

    (define/public (kill)
      (when ptr
        (sfHttpRequest_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfHttpRequest*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (set-field name value)
      (when ptr
        (sfHttpRequest_setField ptr name value)))

    (define/public (set-method method)
      (when ptr
        (sfHttpRequest_setMethod ptr method)))

    (define/public (set-uri uri)
      (when ptr
        (sfHttpRequest_setUri ptr uri)))

    (define/public (set-http-version major minor)
      (when ptr
        (sfHttpRequest_setHttpVersion (abs (inexact->exact (floor major)))
                                      (abs (inexact->exact (floor minor))))))

    (define/public (set-body body)
      (when ptr
        (sfHttpRequest_setBody ptr body)))))

(define (http-request? object-clause)
  (is-a? object-clause http-request%))


(define http-response%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfHttpResponse*? p)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (kill)
      (when ptr
        (sfHttpResponse_destroy ptr)
        (set! ptr #f)))

    (define/public (get-field name)
      (define out #f)
      (when ptr
        (set! out (sfHttpResponse_getField ptr name)))
      out)

    (define/public (get-status)
      (define out #f)
      (when ptr
        (set! out (sfHttpResponse_getStatus ptr)))
      out)

    (define/public (get-major-version)
      (define out #f)
      (when ptr
        (set! out (sfHttpResponse_getMajorVersion ptr)))
      out)

    (define/public (get-minor-version)
      (define out #f)
      (when ptr
        (set! out (sfHttpResponse_getMinorVersion ptr)))
      out)

    (define/public (get-body)
      (define out #f)
      (when ptr
        (set! out (sfHttpResponse_getBody ptr)))
      out)))

(define http%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfHttp_create))))

    (define/public (kill)
      (when ptr
        (sfHttp_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfHttp*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (set-host hostname port)
      (when ptr
        (sfHttp_setHost ptr hostname (abs (inexact->exact (floor port))))))

    (define/public (send-request request timeout)
      (define out (new http-response%))
      (when (and ptr
                 (http-request? request)
                 (time-struct? timeout))
        (send out
              set-pointer
              (sfHttp_sendRequest ptr request timeout))))))

(define (http? object-clause)
  (is-a? object-clause http%))

; ---------------------------------------------------------------------------------------
; Packet.hpp
; ---------------------------------------------------------------------------------------

(define packet%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfPacket_create))))

    (define/public (kill)
      (when ptr
        (sfPacket_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfPacket*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (copy)
      (define out (new packet%))
      (when ptr
        (send out set-pointer (sfPacket_copy ptr)))
      out)

    (define/public (append data size-in-bytes)
      (when ptr
        (sfPacket_append ptr data (inexact->exact (floor size-in-bytes)))))

    (define/public (clear)
      (when ptr
        (sfPacket_clear ptr)))

    (define/public (get-data)
      (define out #f)
      (when ptr
        (set! out (sfPacket_getData ptr)))
      out)

    (define/public (get-data-size)
      (define out #f)
      (when ptr
        (set! out (sfPacket_getDataSize ptr)))
      out)

    (define/public (end-of-packet?)
      (define out #t)
      (when ptr
        (set! out (sfPacket_endOfPacket ptr)))
      out)

    (define/public (can-read?)
      (define out #f)
      (when ptr
        (set! out (sfPacket_canRead ptr)))
      out)

    (define/public (read-bool)
      (define out null)
      (when ptr
        (set! out (sfPacket_readBool ptr)))
      out)

    (define/public (read-int8)
      (define out null)
      (when ptr
        (set! out (sfPacket_readInt8 ptr)))
      out)

    (define/public (read-uint8)
      (define out null)
      (when ptr
        (set! out (sfPacket_readUint8 ptr)))
      out)

    (define/public (read-int16)
      (define out null)
      (when ptr
        (set! out (sfPacket_readInt16 ptr)))
      out)

    (define/public (read-uint16)
      (define out null)
      (when ptr
        (set! out (sfPacket_readUint16 ptr)))
      out)

    (define/public (read-int32)
      (define out null)
      (when ptr
        (set! out (sfPacket_readInt32 ptr)))
      out)

    (define/public (read-uint32)
      (define out null)
      (when ptr
        (set! out (sfPacket_readUint32 ptr)))
      out)

    (define/public (read-float)
      (define out null)
      (when ptr
        (set! out (sfPacket_readFloat ptr)))
      out)

    (define/public (read-double)
      (define out null)
      (when ptr
        (set! out (sfPacket_readDouble ptr)))
      out)

    (define/public (read-string)
      (define out null)
      (when ptr
        (set! out (sfPacket_readString ptr)))
      out)

    (define/public (write-bool b)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeBool ptr b)))
      out)
    
    (define/public (write-int8 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeInt8 ptr i)))
      out)

    (define/public (write-uint8 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeUint8 ptr)))
      out)

    (define/public (write-int16 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeInt16 ptr i)))
      out)

    (define/public (write-uint16 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeUint16 ptr i)))
      out)

    (define/public (write-int32 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeInt32 ptr i)))
      out)

    (define/public (write-uint32 i)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeUint32 ptr i)))
      out)

    (define/public (write-float f)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeFloat ptr f)))
      out)

    (define/public (write-double d)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeDouble ptr d)))
      out)

    (define/public (write-string s)
      (define out null)
      (when ptr
        (set! out (sfPacket_writeString ptr s)))
      out)))

(define (packet? object-clause)
  (is-a? object-clause packet%))

; -----------------------------------------------------------------------------------
; TcpListener.hpp
; -----------------------------------------------------------------------------------

(define tcp-listener%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfTcpListener_create))))

    (define/public (kill)
      (when ptr
        (sfTcpListener_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfTcpListener*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (set-blocking flag)
      (when (and ptr
                 (boolean? flag))
        (sfTcpListener_setBlocking ptr flag)))

    (define/public (blocking?)
      (define out #f)
      (when ptr
        (set! out (sfTcpListener_isBlocking ptr)))
      out)

    (define/public (get-local-port)
      (define out #f)
      (when ptr
        (set! out (sfTcpListener_getLocalPort ptr)))
      out)

    (define/public (listen port address)
      (define out #f)
      (when (and ptr
                 (ip-address? address))
        (set! out (sfTcpListener_listen ptr (abs (inexact->exact (floor port))) address)))
      out)

    (define/public (accept connected)
      (define out #f)
      (when ptr
        (set! out (sfTcpListener_accept ptr connected)))
      out)))

(define (tcp-listener? object-clause)
  (is-a? object-clause tcp-listener%))

; -----------------------------------------------------------------------------------
; TcpSocket.hpp
; -----------------------------------------------------------------------------------

(define tcp-socket%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfTcpSocket_create))))

    (define/public (kill)
      (when ptr
        (sfTcpSocket_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfTcpSocket*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (set-blocking flag)
      (when (and ptr
                 (boolean? flag))
        (sfTcpSocket_setBlocking ptr flag)))

    (define/public (blocking?)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_isBlocking ptr)))
      out)

    (define/public (get-local-port)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_getLocalPort ptr)))
      out)

    (define/public (get-remote-address)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_getRemoteAddress ptr)))
      out)

    (define/public (get-remote-port)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_getRemotePort ptr)))
      out)

    (define/public (connect remote-address remote-port timeout)
      (define out #f)
      (when (and ptr
                 (ip-address? remote-address)
                 (time-struct? timeout))
        (set! out (sfTcpSocket_connect ptr remote-address (abs (inexact->exact (floor remote-port))) timeout)))
      out)

    (define/public (disconnect)
      (when ptr
        (sfTcpSocket_disconnect ptr)))

    (define/public (send-data data size)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_send ptr data size)))
      out)

    (define/public (send-partial data size sent)
      (define out #f)
      (when ptr
        (set! out (sfTcpSocket_send ptr data size sent)))
      out)

    (define/public (send-packet packet)
      (define out #f)
      (when (and ptr
                 (packet? packet))
        (set! out (sfTcpSocket_sendPacket ptr (send packet pointer))))
      out)

    (define/public (receive-packet packet)
      (define out #f)
      (when (and ptr
                 (packet? packet))
        (set! out (sfTcpSocket_receivePacket ptr (send packet ptr))))
      out)))

(define (tcp-socket? object-clause)
  (is-a? object-clause tcp-socket%))

; -----------------------------------------------------------------------------------
; UdpSocket.hpp
; -----------------------------------------------------------------------------------

(define udp-socket%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfUdpSocket_create))))

    (define/public (kill)
      (when ptr
        (sfUdpSocket_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfUdpSocket*? p)
        (send this kill)
        (set! ptr p)))

    (define/public (valid?)
      (if ptr
          #t
          #f))

    (define/public (set-blocking flag)
      (when (and ptr
                 (boolean? flag))
        (sfUdpSocket_setBlocking ptr flag)))

    (define/public (blocking?)
      (define out #f)
      (when ptr
        (set! out (sfUdpSocket_isBlocking ptr)))
      out)

    (define/public (get-local-port)
      (define out #f)
      (when ptr
        (set! out (sfUdpSocket_getLocalPort ptr)))
      out)

    (define/public (bind port address)
      (define out #f)
      (when (and ptr
                 (exact-nonnegative-integer? port)
                 (ip-address? address))
        (set! out (sfUdpSocket_bind ptr port address)))
      out)

    (define/public (unbind)
      (when ptr
        (sfUdpSocket_unbind ptr)))

    (define/public (send-data data size remote-address remote-port)
      (define out #f)
      (when (and ptr
                 (exact-nonnegative-integer? size)
                 (ip-address? remote-address)
                 (exact-nonnegative-integer? remote-port))
        (set! out (sfUdpSocket_send ptr data size remote-address remote-port)))
      out)

    (define/public (receive data size recieved remote-address remote-port)
      (define out #f)
      (when (and ptr
                 (exact-nonnegative-integer? size)
                 (ip-address? remote-address)
                 (exact-nonnegative-integer? remote-port))
        (set! out (sfUdpSocket_receive ptr data size recieved remote-address remote-port)))
      out)

    (define/public (send-packet packet)
      (define out #f)
      (when (and ptr
                 (packet? packet))
        (sfUdpSocket_sendPacket ptr (send packet pointer)))
      out)

    (define/public (receive-packet packet)
      (define out #f)
      (when (and ptr
                 (packet? packet))
        (sfUdpSocket_receivePacket ptr (send packet pointer)))
      out)))

(define (udp-socket? object-clause)
  (is-a? object-clause udp-socket%))
        
; -----------------------------------------------------------------------------------
; SocketSelector.hpp
; -----------------------------------------------------------------------------------
    
(define socket-selector%
  (class object%
    (super-new)
    (field [ptr #f])

    (define/public (make)
      (unless ptr
        (set! ptr (sfSocketSelector_create))))

    (define/public (kill)
      (when ptr
        (sfSocketSelector_destroy ptr)
        (set! ptr #f)))

    (define/public (pointer) ptr)

    (define/public (set-pointer p)
      (when (sfSocketSelector*? p)
        (send this kill)
        (set! ptr p)))
    
    (define/public (valid?)
      (if ptr
          #t
          #f))
    
    (define/public (copy)
      (define out (new socket-selector%))
      (when ptr
        (send out set-pointer (sfSocketSelector_copy ptr)))
      out)

    (define/public (add-tcp-listener listener)
      (when (and ptr
                 (tcp-listener? listener))
        (sfSocketSelector_addTcpListener ptr (send listener pointer))))

    (define/public (add-tcp-socket socket)
      (when (and ptr
                 (tcp-socket? socket))
        (sfSocketSelector_addTcpSocket ptr (send socket pointer))))

    (define/public (add-udp-socket socket)
      (when (and ptr
                 (udp-socket? socket))
        (sfSocketSelector_addUdpSocket ptr (send socket pointer))))

    (define/public (remove-tcp-listener listener)
      (when (and ptr
                 (tcp-listener? listener))
        (sfSocketSelector_removeTcpListener ptr (send listener pointer))))

    (define/public (remove-tcp-socket socket)
      (when (and ptr
                 (tcp-socket? socket))
        (sfSocketSelector_removeTcpSocket ptr (send socket pointer))))

    (define/public (remove-udp-socket socket)
      (when (and ptr
                 (udp-socket? socket))
        (sfSocketSelector_removeUdpSocket ptr (send socket pointer))))

    (define/public (clear)
      (when ptr
        (sfSocketSelector_clear ptr)))

    (define/public (wait timeout)
      (define out #f)
      (when (and ptr
                 (time-struct? timeout))
        (set! out (sfSocketSelector_wait ptr timeout)))
      out)

    (define/public (tcp-listener-ready? listener)
      (define out #f)
      (when (and ptr
                 (tcp-listener? listener))
        (set! out (sfSocketSelector_isTcpListenerReady ptr (send listener pointer))))
      out)

    (define/public (tcp-socket-ready? socket)
      (define out #f)
      (when (and ptr
                 (tcp-socket? socket))
        (set! out (sfSocketSelector_isTcpSocketReady ptr (send socket pointer))))
      out)

    (define/public (udp-socket-ready? socket)
      (define out #f)
      (when (and ptr
                 (udp-socket? socket))
        (set! out (sfSocketSelector_isUdpSocketReady ptr (send socket pointer))))
      out)))

(define (socket-selector? object-clause)
  (is-a? object-clause socket-selector%))

    

