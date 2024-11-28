;; Improve startup time
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Display load message when starting emacs
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
          (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq inhibit-startup-message t)

;; Remove visible bell
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Fonts
(set-face-attribute 'default nil :font "Hack" :height 115)
(set-face-attribute 'fixed-pitch nil :font "Hack" :height 115)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 125)

(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
		dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;;; Vim Bindings
(setq evil-want-keybinding nil)
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package undo-fu
  :demand t
  :commands (undo-fu-only-undo
	     undo-fu-only-redo
	     undo-fu-only-redo-all
	     undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

;; Vim-commentary without any extra package
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;; Protesilaos' ef-theme loading
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-symbiosis t))

;;; Vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map minibuffer-local-map
	 ("C-j" . vertico-next)
	 ("C-k" . vertico-previous)
	 ("C-l" . next-history-element)
	 ("C-h" . previous-history-element))
  :config
  (setq vertico-resize nil
	vertico-count 8
	vertico-cycle t)

  ;; This removes the shadowed-out region when finding file from an
  ;; already arrived directory
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; Vi-style fringes for empty lines
(use-package vi-tilde-fringe
  :demand t
  :config
  (global-vi-tilde-fringe-mode))

;; Disable vi-style fringes for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
		dired-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (vi-tilde-fringe-mode -1))))

;; Which-key (to show available commands when typing a prefix say 'C-c')
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Org-mode specific settings
(use-package org
  :ensure nil
  :config
  (setq org-log-done 'time)
  (setq org-todo-keywords
    '((sequence "TODO" "IN-PROGRESS" "DONE")))
  ;; org-indent-mode turned on by default
  (setq org-startup-indented t)
  ;; Emacs identifies sentences with a single space after fullstop.
  (setq sentence-end-double-space nil)
  ;; Start calendar week from Monday
  (setq calendar-week-start-day 1)

  ;; Org specific global keybindings
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

;; Automatic text wrapping in all major modes
(setq-default auto-fill-function 'do-auto-fill)

;; Magit
(use-package magit
  :ensure t)

(use-package markdown-mode
  :defer t
  :mode ("*.md" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; Deletes trailing whitespace upon saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Vim like scrolling
(setq scroll-step            1
    scroll-conservatively  10000
    next-screen-context-lines 5
    ;; move by logical lines rather than visual lines (better for macros)
    line-move-visual nil)

;; Marginalia package
(use-package marginalia
    :bind (:map minibuffer-local-map
	    ("M-A" . marginalia-cycle))

    :init
    (marginalia-mode))

;; AucTeX package for LaTeX niceties
(use-package tex
  :ensure auctex
  :ensure cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
	(LaTeX-mode . reftex-mode))  ;; Turn on reftex by default in .tex files
  :config
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  (setq font-latex-fontify-script nil))    ;; disables fontification of formatted text

(use-package orderless
  :custom
  ((completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))))

;; ;; Add Zathura as default pdf-viewer
;; (push (list 'output-pdf "Zathura") TeX-view-program-selection)

;; ;; Text wrapping for specific modes
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (set-fill-column 90)))

;; Text wrapping for specific modes
(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun text-wrapper ()
  (lambda ()
    (set-fill-column 90)))

(my-add-to-multiple-hooks
 'text-wrapper
 '(org-mode-hook
   markdown-mode-hook
   LaTeX-mode-hook))

;; vterm terminal emulation
(use-package vterm
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (add-hook 'vterm-mode-hook #'turn-off-evil-mode nil)
  (setq vterm-timer-delay 0.01))


;; World-clock customization
;;;; World clock (M-x world-clock)
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/New_York" "New York")
          ("America/Vancouver" "Vancouver")
          ("UTC" "UTC")
          ("Europe/Berlin" "Berlin")
          ("Asia/Kolkata" "Kolkata")
          ("Asia/Singapore" "Singapore")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Sydney" "Sydney")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;; TODO add consult and embark packages
;; TODO play around with General.el

;; Replace "yes or no" with "y or n"
(setq use-short-answers t)

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;;; Dired (Directory Editor) customizations
(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))


;; Minibuffer keybindings
(define-key minibuffer-local-map (kbd "C-v") 'yank)

;; Put autosave files in one folder
(setq backup-directory-alist `(("." . "~/.autosaves")))

;; Emacs local webserver
(use-package simple-httpd
  :ensure t)

;; Python-mode config
;; Use TAB in place of C-M-i for completion-at-point
;; (setq tab-always-indent 'complete)

;; YAML file support
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
