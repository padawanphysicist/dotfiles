(in-package :stumpwm)

(set-prefix-key (kbd "s-z"))
(which-key-mode)

;; Launcher
(defcommand rofi-drun () () 
	    (run-shell-command "rofi -modi drun,run,ssh -show drun"))
;;(define-key *root-map* (kbd "d") "rofi-drun")
(define-key *top-map*  (kbd  "s-p") "rofi-drun")

;; Terminal
(defcommand terminal () () 
	    (run-shell-command "xterm"))
(define-key *top-map* (kbd "s-S-RET") "terminal")

;; Group navigation
;; Use S-<n> for going to n-th group
(loop for i from 1 to 4 do
  (define-key *top-map*
      (kbd (format nil "s-~a" i))
    (format nil "gselect ~a" i)))
(define-key *top-map* (kbd "s-ESC") "gother")

;; Use S-<n> for going to n-th group
(define-key *top-map*  (kbd  "s-!") "gmove 1")
(define-key *top-map*  (kbd  "s-@") "gmove 2")
(define-key *top-map*  (kbd  "s-#") "gmove 3")
(define-key *top-map*  (kbd  "s-$") "gmove 4")

;; End session
(defvar *my-end-session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "end-session")
    (define-key m (kbd "l") "logout")
    (define-key m (kbd "s") "suspend-computer")
    (define-key m (kbd "S") "shutdown-computer")
    (define-key m (kbd "r") "loadrc")
    (define-key m (kbd "R") "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))
(define-key *root-map* (kbd "q") '*my-end-session-keymap*)
