(require 'package)
(require 'org-macs)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
;; Enable Evil
(require 'evil)
(evil-mode 0)



(setq global-auto-revert-non-file-buffers t)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
;;(load custom-file 'noerror 'nomessage)

(setq history-length 25)
(savehist-mode 1)
;; Remembers your location in file
(save-place-mode 1)

(add-hook 'emacs-startup-hook #'recentf-open-files)

(require 'dired)
(setq dired-recursive-deletes 'top) ;; чтобы можно было непустые директории удалять...

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package magit)
;; for vim-like keybindings in magit
;(use-package evil-collection
 ; :ensure t
  ;:after evil
  ;:init
  ;(evil-collection-init))

(show-paren-mode t) ;; show parentesis{},[],()
;; Delete selection
(delete-selection-mode t)

(electric-indent-mode -1)
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок

(global-set-key (kbd "C-<tab>") 'other-window)
;;change win focus S-<left> etc
(windmove-default-keybindings)

(set-frame-font "Comic Code Demo 20")
  (hl-line-mode nil)

  ;;theme customization
  (setq modus-themes-mode-line '(accented borderless padded))
  (setq modus-themes-region '(bg-only))
  (setq modus-themes-syntax '(faint))
  (setq modus-themes-syntax '(green-strings))

  (setq modus-themes-headings
        '((1 . (raindow  1.3))
           (2 . (raindow  1.2))
           (3 . (raindow  1.1))
          (t . (semilight 1.1))))
  (setq modus-themes-scale-headings t)
  ;(setq modus-themes-org-blocks 'gray-background)
(load-theme 'modus-vivendi t)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq initial-buffer-choice nil)
(setq ingibit-startup-message t) ;; no hello screen
;; Disable GUI components
(tooltip-mode -1)
(menu-bar-mode -1) 
(tool-bar-mode  -1)
(scroll-bar-mode -1) 
(blink-cursor-mode nil) 

(setq use-dialog-box nil)
(setq redisplay-dont-pause t)  ;; better buffer rendering 
(setq ring-bell-function 'ignore) ;; disable bell
;;(setq visible-bell t) ;; disable bell

(recentf-mode 1)

;; Display time in mode-line
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line

;;(global-display-line-numbers-mode nil)
;;(global-visual-line-mode t)
(setq display-line-numbers 'relative)

(use-package org-roam)

(setq org-babel-python-command "python3")
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → " ))
