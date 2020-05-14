(load-file "~/.emacs.d/sensible_defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(require 'server)
(unless (server-running-p)
    (server-start))

(setq user-full-name "gr13nka"
      user-mail-address "gr13nka@yandex.ru")

(add-to-list 'load-path "~/.emacs.d/resources/")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(defun hrs/view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun hrs/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
     (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun hrs/beautify-json ()
  "Pretty-print the JSON in the marked region. Currently shells
     out to `jsonpp'--be sure that's installed!"
  (interactive)
  (Save-excursion
    (shell-command-on-region (mark) (point) "jsonpp" (buffer-name) t)))

(defun hrs/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

(defun hrs/mac? ()
  "Returns `t' if this is an Apple machine, nil otherwise."
  (eq system-type 'darwin))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun hrs/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun hrs/generate-password ()
  "Insert a good alphanumeric password of length 30."
  (interactive)
  (hrs/insert-random-string 30))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(when (hrs/mac?)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(global-prettify-symbols-mode t)

(when window-system
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(when window-system
  (global-hl-line-mode))

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(require 'sr-speedbar)
(add-hook 'sr-speedbar-toggle
          '(lambda ()
	  (sr-speedbar-select-window))
(add-hook 'sr-speedbar-toggle 'sr-speedbar-select-window))

(setq multi-term-program-switches "--login")

(add-hook 'term-mode-hook
	  (lambda ()
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (setq yas-dont-activate t)))

(setq scss-compile-at-save nil)

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))

(setq js-indent-level 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

(setq python-indent 2)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(add-hook 'slim-mode-hook 'rspec-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)))

(hrs/add-auto-mode
 'web-mode
 "\\.erb$"
 "\\.html$"
 "\\.php$"
 "\\.rhtml$")

(add-hook 'yaml-mode-hook 'rspec-mode)

(add-to-list 'load-path "/path/to/directory/where/lua-mode-el/resides")

    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files
      (list (org-file-path "index.org")))

(setq org-capture-templates
      '(
        ("t" "Todo"
         entry
         (file (org-file-path "index.org"))
         "* TODO %?\n")))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done 'time)

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file (org-file-path "index.org"))
  (end-of-buffer))

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(require 'ox-md)
(require 'ox-beamer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

(when (hrs/mac?)
  (setenv "PATH"
          (concat (getenv "PATH")
                  ":" "/usr/local/texlive/2013basic/bin/universal-darwin"
                  ":" "/usr/local/texlive/2013/bin/universal-darwin"
                  ":" "/usr/local/bin")))

(setq dired-dwim-target t)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)

(autoload
'ace-jump-mode-pop-mark
"ace-jump-mode"
"Ace jump back:-)"
t)
(eval-after-load "ace-jump-mode"
'(ace-jump-mode-enable-mark-sync))

(require 'multi-term)
  (setq multi-term-program-switches "--login")
  (setq multi-term-program "/bin/bash")



(defcustom multi-term-buffer-name "terminal"
  "The buffer name of term buffer."
  :type 'string
  :group 'multi-term)

(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'auto-complete-config)
(ac-config-default)
(package-initialize)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(setq yas/indent-line nil)

(smex-initialize)

(add-hook 'markdown-mode-hook 'flyspell-mode)

(hrs/add-auto-mode 'markdown-mode "\\.md$")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(setq wgrep-auto-save-buffer t)

(wrap-region-global-mode t)
(wrap-region-add-wrapper "/" "/" nil 'ruby-mode)
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode))

(defun hrs/split-horizontally-for-temp-buffers ()
  (when (one-window-p t)
    (split-window-horizontally)))

(add-hook 'temp-buffer-window-setup-hook
          'hrs/split-horizontally-for-temp-buffers)

(projectile-global-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun hrs/visit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/configuration.org"))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(require 'engine-mode)

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine yandex
        "https://yandex.ru/search/?lr=213&text=%s"
        :keybinding "y")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "u")

(engine-mode t)

(global-set-key (kbd "C-w") 'backward-kill-word)
  (global-set-key (kbd "M-z") 'other-window)

;;windows split
  (global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
  (global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

  (global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

  (global-set-key (kbd "C-x e") 'hrs/visit-emacs-config)

  (global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-M-w") 'sr-speedbar-close)
  (global-set-key (kbd "M-t") 'shell-pop)

  (define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)

 (global-set-key (kbd "C-c i") 'open-index-file)
 (global-set-key (kbd "M-n") 'org-capture-todo)

  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
  (define-key global-map (kbd "C-q") 'ace-jump-mode)

(global-set-key (kbd "C-c s") 'multi-term)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key [f2] 'kmacro-call-macro)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)
