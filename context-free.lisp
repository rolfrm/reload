;;Context free object manager
;Framework to generate context-free objects
;this is usefull for
;file handling and everything with an OS handle
;also sockets, windows, OpenGL
;Idea is to describe the needed objects like this:
(recieve-bytes 500 :timeout 10000 :server 127.0.0.1) ;receive 500 bytes with timeout 10000 from localhost
;or like this:
(defvar conn (tcp-con :ip 127.0.0.1 :port 12345 :timeout 10000))
(recieve 500 conn)
(send "12345" conn)
(listen 80 :timeout :never :on-connect (lambda (conn) (print conn)))
;However listen would have to be blocking to avoid state

;files
(defvar file (define-file "test" :mode :read-write))
(write-bytes file "1234")
(write-bytes file "5678")
(write-bytes file "91011")
(read-bytes (seek file :position begin))
;writes the bytes to the file, but no closing or opening is needed. Error can happen if another object writes to the file at the same time. A identifier should be kept to all context free object so it will be posible to lock the resource.

;windows
(defvar win (define-window :title "testwindow" :width 100 :height 100 :timeout 10000))
(add-listener (get-value win 'mouse-click-event) (lambda (button action) (print button)))
(with-window win
  ;draw stuff..
  (swap-buffers)
  (pool-events win
	       :mouse-move (lambda (x y) (print ("..")))
	       :key (lambda (key action) (print ".."))
	       :mouse-click (lambda (button action) (print "react on button click")))
  )
;win holds the reference to the window Win can bereplaced by a new window with define-window, but in this case the old window would be removed after a while. In any case, if the window remains inactive for a while, it should be deleted, as with anything unused,
