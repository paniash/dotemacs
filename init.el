;; use-package no longer needed at runtime
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant
(setq use-package-always-ensure t)

;; Display load message when starting emacs
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
          (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Silence byte compile warnings when installing new packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

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
  :init
  ;; Remove visible bell
  (setq visible-bell nil
        ring-bell-function #'ignore)

  ;; Fonts
  (set-face-attribute 'default nil :font "Hack" :height 115)
  (set-face-attribute 'fixed-pitch nil :font "Hack" :height 115)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Term" :height 120)
  :config
  (setq kill-do-not-save-duplicates t)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq tab-always-indent 'complete) ; tab for completion and not indenting
  (setq auto-save-timeout nil)
  (setq help-window-select t) ; Cursor focus goes to help window when invoked
  (setq-default auto-fill-function 'do-auto-fill) ;; Automatic text wrapping in all major modes

  (column-number-mode 1)
  (global-visual-wrap-prefix-mode 1)

  ;; Deletes trailing whitespace upon saving a file
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Vim like scrolling
  (setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)

  ;; Put autosave files in one folder
  (setq backup-directory-alist `(("." . "~/.autosaves")))

  ;; Replace "yes or no" with "y or n"
  (setq use-short-answers t)

  ;; ;; Enable `completion-preview-mode' for certain hooks
  ;; :hook (python-mode . completion-preview-mode)

  (setq display-line-numbers-type 'relative)

  ;; Enable line numbers only for programming and text modes
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  ;; Specify line-spacing (in pixels)
  (setq line-spacing nil)   ;; `nil' is default value

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
  :delight evil-collection-unimpaired-mode
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

;; Protesilaos' ef-themes
(use-package ef-themes
  :ensure t
  :config
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
	ef-themes-variable-pitch-ui t)

  (setq ef-themes-headings ; read the manual's entry or the doc string
	'((0 variable-pitch regular 1.5)
	  (1 variable-pitch regular 1.5)
	  (2 variable-pitch regular 1.25)
	  (3 variable-pitch regular 1.25)
	  (4 variable-pitch regular 1.25)
	  (5 variable-pitch 1.2) ; absence of weight means `bold'
	  (6 variable-pitch 1.2)
	  (7 variable-pitch 1.1)
	  (agenda-date variable-pitch 1.3)
	  (agenda-structure variable-pitch light 1.8)
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
  :ensure nil  ; built-in
  :delight which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Orgmode specific settings
(use-package org
  :ensure nil  ; org is built-in
  :bind (:map org-mode-map
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
	  "~/org/chores.org"
	  "~/org/hobby.org"
	  "~/org/birthdays.org"))

  ;; Faces for TODO keywords
  (setq org-todo-keyword-faces
	'(("PROG" . (:foreground "orange" :weight bold))
	  ("TODO" . (:foreground "#ca80e6" :weight bold))
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

;; Magit
(use-package magit
  :ensure t
  :bind
  ( :map global-map
    ("C-x g" . magit-status)
    :map magit-mode-map
    ("C-w" . nil)
    ("M-w" . nil))
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq git-commit-summary-max-length 70)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-diff-refine-hunk nil))

;; Delight package for hiding minor modes in the modeline
(use-package delight
  :ensure t)

(use-package markdown-mode
  :defer t
  :mode ("*.md" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))

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
(evil-define-key nil 'global
  (kbd "<leader>r") 'consult-ripgrep)
(evil-define-key nil 'global
  (kbd "<leader>l") 'lgrep)

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

;; Minibuffer settings
(use-package minibuffer
  :ensure nil
  :bind
  ( :map minibuffer-local-map
    ("C-v" . yank)))

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

  (setq
   mu4e-drafts-folder "/Drafts"
   mu4e-sent-folder "/Sent"
   mu4e-refile-folder "/Archive"
   mu4e-trash-folder "/Trash")

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

;; ;; sending email in emacs
;; (use-package smtpmail
;;   :ensure nil
;;   :after message
;;   :config
;;   (setq message-send-mail-function 'smtpmail-send-it)
;;   (setq send-mail-function 'smtpmail-send-it)
;;   (setq smtpmail-smtp-server "smtp.migadu.com")
;;   (setq smtpmail-smtp-service 465)
;;   (setq smtpmail-debug-info t)
;;   (setq smtpmail-stream-type 'nil))

;; Notmuch for email
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp/"
  :defer t
  :commands (notmuch notmuch-mua-new-mail)
  :config
  ; General UI
  (setq notmuch-show-logo nil
	notmuch-column-control 1.0
	notmuch-hello-auto-refresh t
	notmuch-hello-recent-searches-max 20
	notmuch-hello-thousands-separator ""
	notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
	notmuch-show-all-tags-list t)

  ; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-show-empty-saved-searches t)

  ; Tags
  (setq notmuch-archive-tags nil ; I don't archive email
	notmuch-message-replied-tags '("+replied")
	notmuch-message-forwarded-tags '("+forwarded")
	notmuch-show-mark-read-tags '("-unread")
	notmuch-draft-tags '("+draft")
	notmuch-draft-folder "drafts"
	notmuch-draft-save-plaintext 'ask)

  ; Email composition
  (setq notmuch-mua-compose-in 'new-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal)
  (setq notmuch-address-use-company nil)
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function
	'message-cite-original-without-signature)
  (setq notmuch-mua-user-agent-function nil)

  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  ;; Prot customization
  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))

  :bind
  ( :map global-map
    ("C-c m" . notmuch)
    ("C-x m" . notmuch-mua-new-mail) ; override `compose-mail'
    :map notmuch-search-mode-map
    ("a" . nil) ; not archiving so better to disable it
    ("A" . nil) ; same reason
    ("/" . notmuch-search-filter) ; alias for l
    ("r" . notmuch-search-reply-to-thread) ; easier to reply to all by default
    ("R" . notmuch-search-reply-to-thread-sender)
    :map notmuch-show-mode-map
    ("a" . nil) ; not archiving so better to disable it
    ("A" . nil)
    ("r" . notmuch-show-reply) ; easier to reply to all by default
    ("R" . notmuch-show-reply-sender)
    :map notmuch-hello-mode-map
    ("C-<tab>" . nil)))

;; Message composition for email
(use-package message
  :ensure nil
  :defer t
  :hook
  (message-setup . message-sort-headers)
  :config
  (setq mail-user-agent 'message-user-agent
	message-mail-user-agent t)  ; use `mail-user-agent'
  (setq mail-header-separator "--text follows this line--")
  (setq message-elide-ellipsis "\n [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-signature "Ashish Panigrahi\nhttps://ashishpanigrahi.com\n"
	mail-signature message-signature)
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format (concat "> From: %f\n"
					     "> Date: %a, %e %b %Y %T %z\n"
					     ">")
	message-ignored-cited-headers "") ; default is "." for all headers
  (setq message-confirm-send nil) ; doesn't ask for confirmation when sending email
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients nil))

(use-package sendmail
  :ensure nil
  :after message
  :config
  (setq send-mail-function 'sendmail-send-it
        ;; ;; NOTE 2023-08-08: We do not need this if we have the Arch
        ;; ;; Linux `msmtp-mta' package installed: it replaces the
        ;; ;; generic sendmail executable with msmtp.
        ;;
        ;; sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header))

;; Add direnv integration in emacs
;; envrc package
(use-package envrc
  :delight
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
  :ensure nil  ;; built-in
  :hook (python-mode . eglot-ensure)
  :config
  ;; (setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
  (setq eglot-ignored-server-capabilities
	'(:documentHighlightProvider :inlayHintProvider :colorProvider
	  :documentLinkProvider))) ;; disables highlighting words under active cursor

;; Eldoc config
(use-package eldoc
  :ensure nil  ;; built-in
  :delight
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
  :bind
  ( :map python-mode-map
    ("C-l" . nil) ; unbind default binding for text view centering
    ("C-l" . xref-go-back)
  )
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq python-shell-enable-font-lock nil)
  (add-hook 'eat-mode-hook #'turn-off-evil-mode nil))

;; Elfeed for RSS
(use-package elfeed
  :ensure t
  :defer t
  :hook ((elfeed-search-mode . variable-pitch-mode)
	 (elfeed-show-mode . variable-pitch-mode))
  :bind (("C-c e" . elfeed)
	 :map elfeed-search-mode-map
	 ("C-c C-c" . elfeed-update))

  :config
  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
    concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))

  (defun pani/my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
	   (title (or (elfeed-meta entry :title)
		      (elfeed-entry-title entry) ""))
	   (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	   (feed (elfeed-entry-feed entry))
	   (feed-title
	    (when feed
	      (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
	   (entry-authors (concatenate-authors
			   (elfeed-meta entry :authors)))
	   (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	   (tags-str (mapconcat
		      (lambda (s) (propertize s 'face
					      'elfeed-search-tag-face))
		      tags ","))
	   (title-width (- (window-width) 10
			   elfeed-search-trailing-width))
	   (title-column (elfeed-format-column
			  title (elfeed-clamp
				 elfeed-search-title-min-width
				 title-width
				 elfeed-search-title-max-width)
			  :left))
	   (authors-width 135)
	   (authors-column (elfeed-format-column
			    entry-authors (elfeed-clamp
					   elfeed-search-title-min-width
					   authors-width
					   80)
			    :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column
			  'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
			  'face 'elfeed-search-date-face
			  'kbd-help entry-authors) " ")
      ;; (when feed-title
      ;;   (insert (propertize entry-authors
      ;; 'face 'elfeed-search-feed-face) " "))
      (when entry-authors
	(insert (propertize feed-title
			    'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))
      )
  )
  (setq elfeed-search-print-entry-function #'pani/my-search-print-fn)
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 20)
  (setq-default elfeed-search-filter "@6-months-ago")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/downloads/")
  (setq elfeed-search-date-format '("%Y-%m-%d" 0 :left))
  (setq elfeed-search-title-max-width 130)
  (setq elfeed-search-title-min-width 120)
  (setq elfeed-search-trailing-width 0))

(use-package elfeed-score
  :ensure t
  :config
  (setq elfeed-score-rule-stats-file "~/.emacs.d/elfeed.stats")
  (setq elfeed-score-serde-score-file "~/.emacs.d/elfeed.score")
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

(use-package elfeed-org
  :ensure t
  :init
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))

;; Hide autorevert mode in modeline
(use-package autorevert
  :delight auto-revert-mode)

;; Info-mode config
(use-package info
  :ensure nil ; built-in
  :hook (Info-mode . variable-pitch-mode))
