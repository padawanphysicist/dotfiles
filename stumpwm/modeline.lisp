(in-package :stumpwm)
(load-module "wifi")

(setf *mode-line-timeout* 2)

(setf *time-modeline-string* "%F %H:%M")

(setf *group-format* "%t")

(setf *window-format* "%n: %30t")

(setf *mode-line-position* :bottom)

(load "~/.stumpwm.d/colors.lisp")

(setf *mode-line-background-color* phundrak-nord1
      *mode-line-foreground-color* phundrak-nord5)

(setf *mode-line-border-color* phundrak-nord1
      *mode-line-border-width* 0)

;;(defvar *mode-line-formatter-list*
;;  '(("%g") ("%W") ("^>") ("mu-unread" . t) ("%m") ("%I") ("%l") ("%C") ("%M") ("%B") ("%d"))
;;  "List of formatters for the modeline.")
;;
;;(defun generate-modeline (elements &optional not-invertedp rightp)
;;  "Generate a modeline for StumpWM.
;;ELEMENTS should be a list of `cons'es which `car' is the modeline
;;formatter or the shell command to run, and their `cdr' is either nil
;;when the `car' is a formatter and t when it is a shell command."
;;  (when elements
;;    (cons (format nil
;;                  " ^[~A^]^(:bg \"~A\") "
;;                  (format nil "^(:fg \"~A\")^(:bg \"~A\")^f1~A^f0"
;;                          (if (xor not-invertedp rightp) phundrak-nord1 phundrak-nord3)
;;                          (if (xor not-invertedp rightp) phundrak-nord3 phundrak-nord1)
;;                          (if rightp "" ""))
;;                  (if not-invertedp phundrak-nord3 phundrak-nord1))
;;          (let* ((current-element (car elements))
;;                 (formatter       (car current-element))
;;                 (commandp        (cdr current-element)))
;;            (cons (if commandp
;;                      `(:eval (run-shell-command ,formatter t))
;;                    (format nil "~A" formatter))
;;                  (generate-modeline (cdr elements)
;;                                     (not not-invertedp)
;;                                     (if (string= "^>" (caar elements)) t rightp)))))))
;;

(defcommand reload-modeline () ()
            "Reload modeline."
	    (setf stumpwm:*screen-mode-line-format*
                  (list "[%n] %v ^>"
                        '(:eval (run-shell-command "echo -n $(uname -r)" t)) " | "
                        '(:eval (run-shell-command "echo -n $(free -t | gawk 'NR==2 {printf(\"RAM: %.1f%%\", $3/$2*100)}')" t)) " | "
                        '(:eval (run-shell-command "get-cpu-usage.sh" t)) " | WiFi: %I | %d")))
(reload-modeline)

