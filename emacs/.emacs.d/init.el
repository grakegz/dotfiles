;; Do not show the startup welcome screen
(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Activate ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set default font
(when (member "Roboto Mono" (font-family-list))
  (set-frame-font "Roboto Mono-11" t t))

;; Some GUI settings I like
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(size-indication-mode t)       ; show size of current file in modeline
(setq visible-bell t)          ; stop that beeping
(setq backup-directory-alist   ; set default directory to keep all autosave files
      `(("." . ,(concat user-emacs-directory "backups"))))
;; I want to start my weeks in the calendar with Monday (default is Sunday)
(setq calendar-week-start-day 1)

;; Try out packages without the need
;; to installed them.
(use-package try
  :ensure t)

;; Show possible key combinations after pressing
;; C-x or M-x
(use-package which-key
  :ensure t
  :config
(which-key-mode))

;; Time for a fancy theme

;; Really like the minimal theme (light version).
;; Will definately try this out for some work.
;; Maybe this will give me lesser distraction and more focus on text?
;; https://github.com/anler/minimal-theme
;;(load-theme 'minimal-light t)

;; https://github.com/tmalsburg/tango-plus-theme
;;(load-theme 'tango-plus t)

;; https://github.com/dgutov/espresso-theme
;;(load-theme 'espresso t)

;; This one is definately a candidate to use more often
;; if not exclusively!
;; https://github.com/jimeh/twilight-bright-theme.el
;;(load-theme 'twilight-bright t)
;; Also if I would like to use a dark theme once in a while
;; here is the counterpart to the light theme.
;; https://github.com/jimeh/twilight-anti-bright-theme
;;(load-theme 'twilight-anti-bright t)

;; Maybe I'll try a couple of base16 themes.
;; I used to like some on vim, so why not.
;; https://github.com/belak/base16-emacs

(use-package twilight-bright-theme
  :ensure t
  ;;:config (load-theme 'twilight-bright t)
)

(use-package twilight-anti-bright-theme
  :ensure t
  :config (load-theme 'twilight-anti-bright t)
)

;; It's time for Magit
(use-package magit
  :ensure t)

;; Some org mode configuration.
;; First fixing problem with built-in org that
;; prevents org archive to be used.
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)
(use-package org
  :ensure t
  :pin org
  :config
  (setq org-startup-indented t))

(require 'org-tempo)  ;; to bring back easy templates using <s or <n

;; Set languages that can be evaluated in org-mode code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (org . t)
   ))

;; Let's try a fancy Dashboard to get an overview
;; everytime we start emacs
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; A handy code formatter for elisp
;; I tried format-all but it didn't do anything to my elisp code?
(use-package elisp-format
  :ensure t)

;; Some personal keybindings
(global-set-key (kbd "<M-s-up>") 'shrink-window)
(global-set-key (kbd "<M-s-down>") 'enlarge-window)
(global-set-key (kbd "<M-s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-s-right>") 'enlarge-window-horizontally)






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   '(org-tempo which-key use-package twilight-bright-theme twilight-anti-bright-theme try org olivetti magit doom-themes doom-modeline dashboard)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
