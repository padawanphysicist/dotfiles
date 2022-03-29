(when *initializing*
  (grename "1:www")
  (gnewbg "2:dev")
  (gnewbg "3:read")
  (gnewbg "4:aux"))

(clear-window-placement-rules)

(define-frame-preference "1:www" (nil t t :class "Firefox"))

(setf *dynamic-group-master-split-ratio* 1/2)

;;(load-module "swm-gaps")
;;
;;(setf swm-gaps:*head-gaps-size* 1 ;; Head gaps run along the 4 borders of the monitor(s)     
;;      swm-gaps:*inner-gaps-size* 5 ;; Inner gaps run along all the 4 borders of a window
;;      ;; Outer gaps add more padding to the outermost borders of a window (touching
;;      ;; the screen border)
;;      swm-gaps:*outer-gaps-size* 5)
;;
;;(when *initializing*
;;  (swm-gaps:toggle-gaps))
