(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-enable-at-startup nil)

;; Set default font
(when (member "Fira Code" (font-family-list))
  (set-frame-font "Fira Code-11" t t))

;; Some GUI settings I like
(setq inhibit-startup-message t)  ;; don't show the default welcome screen
(tool-bar-mode -1)
;; (setq cursor-type 'hbar)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(size-indication-mode t)          ;; show size of current file in modeline
(setq visible-bell t)             ;; stop that beeping

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
(load-theme 'leuven-dark t)

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
  :straight t)

;; get latest org mode
(use-package org
  :straight t
  :config (setq org-startup-indented t))

;; Set languages that can be evaluated in org-mode code blocks
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (emacs-lisp .t)
    (shell . t)
    (org . t)))

;; Let's try a fancy Dashboard to get an overview
;; everytime we start emacs
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

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
