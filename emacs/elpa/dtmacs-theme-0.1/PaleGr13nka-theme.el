;;; PaleGr13nka-theme.el --- PaleGr13nka
;;; Version: 1.0
;;; Commentary:
;;; A theme called PaleGr13nka
;;; Code:

(deftheme PaleGr13nka "DOCSTRING for PaleGr13nka")
  (custom-theme-set-faces 'PaleGr13nka
   '(default ((t (:foreground "#9d9d9d" :background "#303030" ))))
   '(cursor ((t (:background "#a7a7a7" ))))
   '(fringe ((t (:background "#323030" ))))
   '(mode-line ((t (:foreground "#313131" :background "#919789" ))))
   '(region ((t (:background "#454545" ))))
   '(secondary-selection ((t (:background "#464646" ))))
   '(font-lock-builtin-face ((t (:foreground "#cccccc" ))))
   '(font-lock-comment-face ((t (:foreground "#7f857a" ))))
   '(font-lock-function-name-face ((t (:foreground "#9fba93" ))))
   '(font-lock-keyword-face ((t (:foreground "#d6d6d6" ))))
   '(font-lock-string-face ((t (:foreground "#aeb066" ))))
   '(font-lock-type-face ((t (:foreground "#d4d4d4" ))))
   '(font-lock-constant-face ((t (:foreground "#af7383" ))))
   '(font-lock-variable-name-face ((t (:foreground "#83a598" ))))
   '(minibuffer-prompt ((t (:foreground "#7fa251" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'PaleGr13nka)

;;; PaleGr13nka-theme.el ends here
