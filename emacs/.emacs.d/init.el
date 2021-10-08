(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-enable-at-startup nil)

;; Set default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font "DejaVu Sans Mono-11" t t))

;; Some more spacing between the lines for better reading experience.
;; Have to find the sweetspot in the next couple of weeks. 2-4 seems kind of good.
(setq-default line-spacing 2)

;; Some GUI settings I like
(setq inhibit-startup-message t)  ;; don't show the default welcome screen
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (setq cursor-type 'hbar)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(size-indication-mode t)          ;; show size of current file in modeline
(setq visible-bell t)             ;; stop that beeping
(delete-selection-mode t)         ;; overwrite selected text

;; Start emacs in fullscreen mode
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Little bit of transparancy for the emacs gui window :)
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; set default directory to keep all autosave files (don't know if this is really working as expected)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; I want to start my weeks in the calendar with Monday (default is Sunday)
(setq calendar-week-start-day 1)

;; Activate ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Configure flyspell in text-mode buffer and buffers derived from it.
;; But disable it for log-edit-mode and change-log-mode.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Bootstrap straight.el to use as package manager

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file) 
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package to configure and load packages
(straight-use-package 'use-package)

;; Get some themes I like
(use-package minimal-theme
  :straight t
  ;; :config (load-theme 'minimal-light t)
  )

(use-package twilight-bright-theme
  :straight t
  ;; :config (load-theme 'twilight-bright t)
  )

(use-package twilight-anti-bright-theme
  :straight t
  ;; :config (load-theme 'twilight-anti-bright t)
  )

(use-package leuven-theme
  :straight t)

;; Enable a theme for now
;; TODO: replace this with a toggle function to quickly change themes (dark to bright or vice versa)
;; (load-theme 'minimal t)
(load-theme 'twilight-anti-bright t)

;; Try out vertical mode for ido. I think I'll like it better than the horizontal presentation.
(use-package ido-vertical-mode
  :straight t
  :config (ido-vertical-mode 1))

;; Show available shortcuts after pressing C-x or C-c
(use-package which-key
  :straight t
  :config (which-key-mode 1))

;; Use magit for git
(use-package magit
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :straight t)

;; Olivetti to center text in buffer if needed
(use-package olivetti
  :straight t
  :init
  (setq olivetti-body-width 100))

;; get latest org mode
(use-package org
  :straight t
  :config (setq org-startup-indented t))

;; Want to replace the standard folded character sequence ... in orgmode
;; For that I have to set the org-ellipsis variable like:
;; - from standard (setq org-ellipsis "...")
;; - to (setq org-ellipsis "⤵")
;; ▼ ↘ ⤵ ↴ ↷ ⇊ ⇓ ⇘ ⤸ ⤼ ⬎ ↓

;; Set languages that can be evaluated in org-mode code blocks
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (emacs-lisp .t)
    (shell . t)
    (org . t)))

;; I don't want to confirm every time I execute a elisp code block in orgmode
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp")))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

;; Nice bullets
(use-package org-superstar
  :straight t
  :config
  (setq org-superstart-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
			     (org-superstar-mode 1))))

;; Completion framework
(use-package company
  :straight t
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 1.0)
  :hook (prog-mode . company-mode))

;; Popup frame at point, needed by go-translate
(use-package posframe
  :straight t)

;; Google Translate service in Emacs buffer
;; Need to read: https://github.com/lorniu/go-translate
(use-package go-translate
  :straight t
  :config
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-local-language "en")
  (setq go-translate-target-language "de")
  (setq go-translate-buffer-follow-p t))

;; Some personal keybindings
(global-set-key "\C-cT" 'go-translate)
(global-set-key "\C-ct" 'go-translate-popup)
(global-set-key (kbd "<M-s-up>") 'shrink-window)
(global-set-key (kbd "<M-s-down>") 'enlarge-window)
(global-set-key (kbd "<M-s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-s-right>") 'enlarge-window-horizontally)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(org-document-title ((t (:inherit outline-8 :extend nil :height 1.5))))
 ;; '(org-document-info ((t (:inherit org-document-info :extend nil :height 1.2))))
 ;; '(org-document-info-keyword ((t (:inherit org-document-info-keyword :extend nil :height 1.2))))
 ;; '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3))))
 ;; '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2))))
 ;; '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1))))
 ;; '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.0))))
 ;; '(org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0))))
 ;; '(org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0))))
 ;; '(org-level-7 ((t (:inherit outline-7 :extend nil :height 1.0))))
 ;; '(org-level-8 ((t (:inherit outline-8 :extend nil :height 1.0))))
)
