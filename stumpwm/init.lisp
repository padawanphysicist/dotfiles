;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "Programs/Quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :slynk)
(defcommand start-sly-server () ()
  "Start a SLYNK server for SLY"
  (slynk:create-server :dont-close t))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; Autostart
(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "setxkbmap br")
(run-shell-command "~/.fehbg")
(run-shell-command "picom")

;; Load modules
(load-module "end-session") ;; Gracefully end programs when ending user session


;;(load "~/.stumpwm.d/bluetooth.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/placement.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/theme.lisp")
(load "~/.stumpwm.d/utilities.lisp")
(load "~/.stumpwm.d/modeline.lisp")

(setf *mouse-focus-policy*    :click
      *float-window-modifier* :META)

(when *initializing*
  (mode-line))
