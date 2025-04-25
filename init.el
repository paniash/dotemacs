;; Display load message when starting emacs
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
          (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Remove visible bell
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Fonts
(set-face-attribute 'default nil :font "Hack" :height 115)
(set-face-attribute 'fixed-pitch nil :font "Hack" :height 115)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 120)

(setq display-line-numbers-type 'relative)

;; Enable line numbers only for programming and text modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

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

;; Silence byte compile warnings when installing new packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode 1)

;; Disable line numbers for some text modes
(dolist (mode '(eat-mode-hook
                shell-mode-hook
		dired-mode-hook
		woman-mode-hook
		Info-mode-hook
		Man-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Disable certain emacs features
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;;; Some standard emacs config
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq kill-do-not-save-duplicates t)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq tab-always-indent 'complete) ; tab for completion and not indenting
  (setq auto-save-timeout nil)

  ;; (desktop-save-mode 1)

  ;; Enable `completion-preview-mode' for certain hooks
  :hook (python-mode . completion-preview-mode)

  :bind
  ( :map global-map
    ("C-x C-d" . nil) ; never use it
    ("C-x C-z" . nil) ; never use it
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-h h" . nil) ; never show that "hello" file
    ("C-x <left>" . nil) ; unbind the `previous-buffer' command
    ("C-x C-p" . previous-buffer)  ; rebind `previous-buffer' command
    ("C-u" . evil-scroll-up) ; explicitly bind C-u because it sometimes misbehaves in evil mode
    )
  :custom
  ;; disable Ispell completion function
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  )

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  (setq evil-search-module 'isearch)
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

;; Protesilaos' ef-theme
(use-package ef-themes
  :ensure t
  :config
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui t)

  (setq ef-themes-headings ; read the manual's entry or the doc string
	'((0 variable-pitch regular 1.5)
	  (1 variable-pitch regular 1.5)
	  (2 variable-pitch regular 1.2)
	  (3 variable-pitch regular 1.2)
	  (4 variable-pitch regular 1.2)
	  (5 variable-pitch 1.2) ; absence of weight means `bold'
	  (6 variable-pitch 1.2)
	  (7 variable-pitch 1.1)
	  (t variable-pitch 1.1)))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-autumn :no-confirm))

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
  :ensure nil  ; org is built-in
  :bind (:map org-mode-map
	      ("C-c C-r" . consult-ripgrep)
	      ("C-x C-a" . org-archive-subtree-default))
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

  ;; Turn on cdlatex minor mode in all org buffers
  ;; See https://orgmode.org/manual/CDLaTeX-mode.html for details
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  ;; Set renderer for LaTeX preview in orgmode
  (setq org-preview-latex-default-process 'imagemagick)

  ;; Setting org-agenda file
  ;; Eliminates the need for putting org-agenda file to the top everytime
  (setq org-agenda-files
	'("~/org/agenda.org"
	  "~/org/misc.org"
	  "~/org/birthdays.org"))

  ;; Faces for TODO keywords
  (setq org-todo-keyword-faces
	'(("PROG" . "orange")
	  ("TODO" . "#cc43a5")
	  ("CANCELLED" . (:foreground "#B50741" :weight bold))))

  ;; Block parent TODO to DONE if children are undone
  (setq org-enforce-todo-dependencies t)

  ;; Show markup elements (default behaviour)
  (setq org-hide-emphasis-markers nil)

  ;; Add syntax highlighting for org documents
  ;; Also add native <Tab> behaviour in codeblocks
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t)

  ;; Org specific global keybindings
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture))

;; Org-roam for roam research note taking
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package citar
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-bibliography '("~/org/phd-notes/references.bib"))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map ("C-c b" . #'citar-insert-citation))
  (:map LaTeX-mode-map ("C-c b" . #'citar-insert-citation)))

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
  ;; Set Zathura as the default pdf viewer
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  (add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "LaTeXmk")))
  (setq font-latex-fontify-script nil))    ;; disables fontification of formatted text

;; Setup YaSnippet for LaTeX
(use-package yasnippet
  :ensure t
  :hook (LaTeX-mode . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package orderless
  :custom
  ((completion-styles '(orderless basic))
  (completon-category-overrides '((file (styles basic partial-completion))))))

;; Setting leader key in emacs
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal LaTeX-mode-map
  (kbd "<leader>c") 'TeX-command-run-all)
(evil-define-key nil 'global
  (kbd "<leader>o") 'toggle-window-split)
(evil-define-key nil 'global
  (kbd "<leader>b") 'bookmark-jump)

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
  (add-hook 'eat-mode-hook (lambda () (evil-local-mode -1))))

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
(use-package consult
  :ensure t)

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
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t))

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

;; Python-mode config
;; Use TAB in place of C-M-i for completion-at-point
;; (setq tab-always-indent 'complete)
(setq python-shell-enable-font-lock nil)

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

;; Mainly for running some inferior process that doesn't require a lot
;; of screen real estate (`inferior-python' for example).
;; TODO: Really understand this function.
(defun pani/my-split-window-below-30 (&optional window-to-split)
  "Split WINDOW-TO-SPLIT into two windows, with the lower window occupying ~30% of the height.
WINDOW-TO-SPLIT defaults to the selected window.
Returns the new window."
  (interactive)
  (let* ((window (or window-to-split (selected-window)))
         (total-height (window-total-height window))
         (size (- (floor (* total-height 0.3)))))
    (unless (>= (- total-height size) window-min-height)
      (error "Resulting window too small"))
    (split-window window size)))

;; mu4e for email
(use-package mu4e
  :ensure nil
  :config

  ;; This is set to `t' to avoid mail syncing issues when using mbsync
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

;; Add direnv integration in emacs
;; envrc package
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
        (python-mode . envrc-global-mode))

;; Autocompletion via corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t) ; enable cycling for `corfu-next/previous'

  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :init
  (global-corfu-mode))

;; LSP server using eglot
(use-package eglot
  :defer t
  :hook (python-mode . eglot-ensure)
  :config
  ;; (setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
  (setq eglot-ignored-server-capabilities
	'(:documentHighlightProvider))) ;; disables highlighting words under active cursor

;; eldoc config
(use-package eldoc
  :defer t  ;; built-in
  :config
  ;; disables resizing of echo area for automatic eldoc documentation
  ;; under the cursor
  (setq eldoc-echo-area-use-multiline-p nil))

;; `proced' (process monitor, similar to `top')
;; This is a built-in emacs package
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible)
  (setq proced-enable-color-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;; Python config
(use-package python
  :ensure nil   ; because this is built-in
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "ipython")
  (add-hook 'eat-mode-hook #'turn-off-evil-mode nil))

;; Elfeed for RSS
(use-package elfeed
  :ensure t
  :bind (("C-c e" . elfeed)
	 :map elfeed-search-mode-map
	 ("C-c C-c" . elfeed-update))

  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 20)
  (setq-default elfeed-search-filter "@6-months-ago +unread")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/downloads/")
  (setq elfeed-feeds
	'("https://www.archlinux.org/feeds/news/"
	  ("https://emacsredux.com/atom.xml" emacs)
	  "https://peterwittek.com/feeds/all.atom.xml"
	  "https://gregorygundersen.com/feed.xml"
	  "https://mikeinnes.github.io/feed.xml"
	  "https://rosenzweig.io/feed.xml"
	  ("https://karthinks.com/index.xml" emacs blog)
	  "https://adol.pw/index.xml"
	  ("https://marci.gunyho.com/rss" blog)
	  "https://andreyor.st/feed.xml"
	  "https://www.paritybit.ca/feed.xml"
	  "https://m-malinowski.github.io/feed.xml"
	  "https://kishvanchee.com/index.xml"
	  "https://dataswamp.org/~solene/rss.xml"
	  "https://drewdevault.com/blog/index.xml"
	  ("https://11de784a.github.io/feed.xml" blog)
	  "https://yarmo.eu/atom.xml"
	  ("https://timharek.no/rss.xml" blog)
	  "https://terrytao.wordpress.com/feed/"
	  ("https://protesilaos.com/commentary.xml" emacs blog)
	  ("https://ogbe.net/blog-feed.xml" emacs)
	  ("https://themkat.net/feed.xml" emacs)
	  "https://www.romanzolotarev.com/rss.xml"
	  ("https://nullprogram.com/feed/" emacs)
	  "https://www.unixsheikh.com/feed.rss"
	  "https://matt.might.net/articles/feed.rss"
	  ("https://lambdaland.org/index.xml" emacs blog)
	  "https://simon.peytonjones.org/feed.xml"
	  "https://jvns.ca/atom.xml"
	  "http://100r.co/links/rss.xml"
	  "https://anchor.fm/s/c761a04/podcast/rss"
	  ("https://phdcomics.com/gradfeed.php" comics)
	  "https://www.noendcomic.com/rss.php"
	  ("https://xkcd.com/atom.xml" comics)
	  ("https://www.smbc-comics.com/comic/rss" comics)
	  ("https://www.commitstrip.com/en/feed/" comics))))

;; Testing sly package for common lisp
(use-package sly
  :defer t)

;; rg.el for ripgrep in Emacs
(use-package rg
  :ensure t)
