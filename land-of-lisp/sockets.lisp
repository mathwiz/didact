;; Create 2 instances of CLISP

; SERVER-SIDE
(defparameter my-socket (socket-server 4321))

(defparameter my-stream (socket-accept my-socket))

(read my-stream)

; bidirectional
(print "Hi Client!" my-stream)

(close my-stream)

(socket-server-close my-socket)


; CLIENT-SIDE
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))

(print "Yo Server!" my-stream)

(read my-stream)

(close my-stream)
