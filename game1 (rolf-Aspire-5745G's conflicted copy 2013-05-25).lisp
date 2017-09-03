;;Continuation demo. (includes everything until now, may 24 2013)
;;This file loads all the code and runs it

(load "/home/rolf/quicklisp/setup.lisp")
(ql:quickload 'alexandria)
(use-package 'alexandria)
(ql:quickload 'cffi)
(ql:quickload 'cl-opengl)
(ql:quickload 'png)
(load "reload-engine.lisp")
(load "reloader.lisp")
(reset-reloader)

(start-new-dispatcher-thread dispatch)
(re.load "qload.lisp")
(re.load "utils.lisp")
(re.load "engine.lisp")
(re.load "game-object.lisp")
