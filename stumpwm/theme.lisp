(load "~/.stumpwm.d/colors.lisp")

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(clx-truetype:cache-fonts)

(set-border-color        phundrak-nord1)
(set-focus-color         phundrak-nord1)
(set-unfocus-color       phundrak-nord3)
(set-float-focus-color   phundrak-nord1)
(set-float-unfocus-color phundrak-nord3)

(set-fg-color phundrak-nord4)
(set-bg-color phundrak-nord1)

;;(setf *normal-border-width*       1
;;      *float-window-border*       1
;;      *float-window-title-height* 15
;;      *window-border-style*       :none
;;      *window-format*             "%n:%t")

;;(set-font `(,(make-instance 'xft:font :family "Unifont-JP" :subfamily "Regular" :size 10 :antialias t)
;;            ,(make-instance 'xft:font :family "DejaVu Sans Mono for Powerline" :subfamily "Book" :size 8.5 :antialias t)
;;            ,(make-instance 'xft:font :family "siji" :subfamily "Medium" :size 10 :antialias t)))
;;
(set-font `(,(make-instance 'xft:font :family "Noto Sans Mono" :subfamily "Regular" :size 13 :antialias t)))
