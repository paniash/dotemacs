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
		woman-mode-hook
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
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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

;; Which-key (to show available commands when typing a prefix say 'C-c')
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Orgmode specific settings
(use-package org
  :ensure nil
  :config
  (setq org-log-done 'time)
  ;; Collapse the log entries into a "drawer"
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "PROG(p)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; org-indent-mode turned on by default
  (setq org-startup-indented t)
  ;; Emacs identifies sentences with a single space after fullstop.
  (setq sentence-end-double-space nil)
  ;; Start calendar week from Monday
  (setq calendar-week-start-day 1)

  ;; Setting org-agenda file
  ;; Eliminates the need for putting org-agenda file to the top everytime
  (setq org-agenda-files
	'("~/org/agenda.org"
	  "~/org/misc.org"
	  "~/org/birthdays.org"))

  ;; Faces for TODO keywords
  (setq org-todo-keyword-faces
	'(("PROG" . "orange")
	  ("CANCELLED" . (:foreground "#B50741" :weight bold))))

  ;; Block parent TODO to DONE if children are undone
  (setq org-enforce-todo-dependencies t)

  ;; Add syntax highlighting for org documents
  ;; Also add native <Tab> behaviour in codeblocks
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t)

  ;; Org specific global keybindings
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

;; (defun open-journal-file ()
;;   (let* ((today (format-time-string "%Y%m%d"))
;;          (path (concat (getenv "HOME") "~/org/phd-notes/logs/" today ".org"))
;;          (hdr-list (list (concat "#+TITLE: [" today "]")
;;                          "#+AUTHOR: Ashish Panigrahi"))
;;          (hdr (apply 'concat
;;                      (mapcar (lambda (s) (concat s "\n"))
;;                              hdr-list)))
;;          (has-hdr (lambda ()
;;                     (save-excursion
;;                       (goto-char (point-min))
;;                       (search-forward "#+TITLE" nil t)))))
;;     (message (concat "opening " path " ..."))
;;     (find-file path)
;;     (unless (funcall has-hdr)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (insert hdr)))
;;     (message "Enjoy your journaling!")))

;; (define-key global-map (kbd "C-c 1")
;; 	    (lambda ()
;; 	      (interactive)
;; 	      (open-journal-file)))

;; Org-roam for roam research note taking
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

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
  :hook ((LaTeX-mode . electric-pair-mode)
	 (LaTeX-mode . cdlatex-mode)
	 (LaTeX-mode . reftex-mode))  ;; Turn on reftex by default in .tex files
  :config
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  ;; LaTeX document parsing enabled (even on save)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Autosave upon compilation
  (setq TeX-save-query nil)
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk")))
  (setq font-latex-fontify-script nil))    ;; disables fontification of formatted text

;; Setup YaSnippet for LaTeX
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package orderless
  :custom
  ((completion-styles '(orderless basic))
  (completon-category-overrides '((file (styles basic partial-completion))))))

;; ;; Add Zathura as default pdf-viewer
;; (push (list 'output-pdf "Zathura") TeX-view-program-selection)
(setq TeX-view-program-selection '((output-pdf "Zathura")))

;; Setting leader key in emacs
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal LaTeX-mode-map
  (kbd "<leader>c") 'TeX-command-run-all)
(evil-define-key nil 'global
  (kbd "<leader>o") 'toggle-window-split)
(evil-define-key nil 'global
  (kbd "<leader>a")
  (lambda () (interactive) (find-file "~/org/agenda.org")))

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

;; Emacs eat terminal emulation
(use-package eat
  :ensure t
  :config
  (add-hook 'eat-mode-hook #'turn-off-evil-mode nil))

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
          ("UTC" "UTC")
          ("Europe/Berlin" "Aachen")
          ("Europe/Helsinki" "Helsinki")
          ("Asia/Kolkata" "Chennai")
          ("Asia/Singapore" "Singapore")
          ("Asia/Tokyo" "Tokyo")
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
(setq python-shell-enable-font-lock nil)

;; YAML file support
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Disable byte-compile warnings during package installation
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Function to instantly toggle between vertical and horizontal split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; mu4e for email
(use-package mu4e
  :ensure nil
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  (setq user-mail-address "ap@ashishpanigrahi.com")
  (setq user-full-name "Ashish Panigrahi")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-trash-folder "/Trash")

  ;; some customizations
  (setq
   mu4e-attachment-dir "~/downloads"
   mu4e-view-show-images t
   mu4e-compose-signature-auto-include nil
   mu4e-view-show-addresses t
   mu4e-hide-index-messages t
   mu4e-compose-dont-reply-to-self t)

  (setq mu4e-maildir-shortcuts
	'(("/INBOX" . ?i)
	  ("/Sent" . ?s)
	  ("/Trash" . ?t)
	  ("/Draft" . ?d))))


;; elpy for python
(use-package elpy
  :defer t
  :commands (elpy-enable elpy-disable)
  :init
  (elpy-enable)
  :config
  ;; disable highlight-indentation minor mode
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))
