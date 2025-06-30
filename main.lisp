(load (sb-ext:posix-getenv "ASDF"))
(in-package :cl)
(defvar *user-config* '())
(let ((config (sb-ext:posix-getenv "CONFIG")))
  (if config
      (load config)
    (progn
      (setf *user-config* '()))))
(export '*user-config*)
(declaim (optimize (speed 3) (space 0) (debug 0)))
(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
(push #p"./" asdf:*central-registry*)

(asdf:load-system :nougat-web)
(use-package :nougat-web)
