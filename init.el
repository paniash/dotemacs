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
  :delight auto-fill-function
  :init
  ;; Remove visible bell
  (setq visible-bell nil
        ring-bell-function #'ignore)

  ;; Fonts
  (set-face-attribute 'default nil :font "Hack" :height 115)
  (set-face-attribute 'fixed-pitch nil :font "Hack" :height 1.0)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Term" :height 1.05)

  :config
  (setq kill-do-not-save-duplicates t)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq tab-always-indent 'complete) ; tab for completion and not indenting
  (setq auto-save-timeout nil)
  (setq help-window-select t) ; Cursor focus goes to help window when invoked
  (setq-default auto-fill-function 'do-auto-fill) ;; Automatic text wrapping in all major modes
  (setq compilation-scroll-output t) ; scroll compilation buffer as output appears

  ;; Automatically close successful build window
  (defun ar/compile-autoclose (buffer string)
    "Hide successful builds window with BUFFER and STRING."
    (if (string-match "finished" string)
	(progn
	  (message "Build finished :)")
	  (run-with-timer 3 nil
			  (lambda ()
			    (when-let* ((multi-window (> (count-windows) 1))
					(live (buffer-live-p buffer))
					(window (get-buffer-window buffer t)))
			      (delete-window window)))))
      (message "Compilation %s" string)))

  (setq compilation-finish-functions (list #'ar/compile-cache-env #'ar/compile-autoclose))

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

  ;; Disable remote file locks
  (setq remote-file-name-inhibit-locks t)

  ;; Put autosave files in one folder
  (setq backup-directory-alist `(("." . "~/.autosaves")))

  ;; Replace "yes or no" with "y or n"
  (setq use-short-answers t)

  ;; isearch now shows number of search hits
  (setq isearch-lazy-count t)

  ;; ;; Enable `completion-preview-mode' for certain hooks
  ;; :hook (python-mode . completion-preview-mode)

  (setq display-line-numbers-type 'relative)

  ;; Enable line numbers only for programming and text modes
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  ;; Specify line-spacing (in pixels)
  (setq line-spacing nil)   ;; `nil' is default value
  (setq tab-bar-show 1) ; turns off `tab-bar-mode' when only 1 tab is present.
  (setq tab-bar-close-button-show nil) ; removes 'x' button from the tabs.
  (setq tab-bar-new-button-show nil) ; removes '+' button for new tab.

  ;; Specify gpg executable
  (setq epg-gpg-program "gpg2")
  ;; ;; Do gpg pin entry from Emacs
  ;; (setenv "GPG_AGENT_INFO" nil)

  ;; Set regexp syntax to `string'
  ;; `read' is default but is plagued with requiring many backslashes
  (setq reb-re-syntax 'string)

  ;; Set default pdf-viewer in org-mode
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "firefox \"%s\""))

  :bind
  ( :map global-map
    ("C-x C-d" . nil) ; never use it
    ("C-x C-z" . nil) ; never use it
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-h h" . nil) ; never show that "hello" file
    ("C-l" . nil) ; never use it
    ("C-x <left>" . nil) ; unbind the `previous-buffer' command
    ("C-x C-p" . previous-buffer)  ; rebind `previous-buffer' command
    ("C-u" . evil-scroll-up) ; explicitly bind C-u because it sometimes misbehaves in evil mode
    )
  :custom
  ;; disable Ispell completion function
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  (setq evil-search-module 'isearch)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)  ;; yanks to end of line instead of whole line
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

;; Setting leader key in emacs
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal LaTeX-mode-map
(kbd "<leader>c") 'TeX-command-run-all)
(evil-define-key nil 'global
(kbd "<leader>o") 'toggle-window-split)
(evil-define-key nil 'global
(kbd "<leader>b") 'consult-buffer)
(evil-define-key nil 'global
(kbd "<leader>r") 'consult-ripgrep)
(evil-define-key nil 'global
(kbd "<leader>l") 'lgrep)

;; Set `t' as a prefix key for tab manipulation commands
(define-prefix-command 'pani/t-key)
(define-key evil-normal-state-map (kbd "t") 'pani/t-key)
(define-key pani/t-key (kbd "j") 'tab-previous)
(define-key pani/t-key (kbd "k") 'tab-next)
(define-key pani/t-key (kbd "n") 'tab-new)
(define-key pani/t-key (kbd "x") 'tab-close)
(define-key pani/t-key (kbd "X") 'tab-close-other)

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
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  ;; They are nil by default...
  (setq modus-themes-mixed-fonts t
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-variable-pitch-ui t)

  ;; Sets line number color back to white
  (setq modus-themes-common-palette-overrides
	'((fg-line-number-active fg-main)))

  ;; Minibuffer completions are bold by default. This fixes it.
  (setq modus-themes-completions
	'((selection regular)))

  (setq modus-themes-headings ; read the manual's entry or the doc string
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
  (load-theme 'ef-symbiosis :no-confirm))

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
  :delight org-cdlatex-mode
  :delight org-indent-mode
  :init
  (setq org-directory (expand-file-name "~/org/"))
  (setq org-imenu-depth 7)
  :hook ((org-mode . auto-revert-mode)
	 (org-agenda-mode . variable-pitch-mode))
  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      ("C-c c" . org-capture)
	 :map org-mode-map
	 ("C-x a" . org-archive-subtree-default)
	 ("C-x i" . org-toggle-inline-images))
  :config
  (use-package org-indent
    :ensure nil
    :delight)
  (setq org-log-done 'time)
  ;; Collapse the log entries into a "drawer"
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
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

  ;; Setup org-capture templates
  (setq org-capture-templates
	`(("i" "Inbox" entry (file "inbox.org")
	   ,(concat "* TODO %?\n"
		    "/Entered on/ %U"))))

  ;; Small hook to tell org-capture to use full window instead of splitting window
  (add-hook 'org-capture-mode-hook 'delete-other-windows)

  ;; Stolen from Nicholas Rougier's GTD guide
  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))

  :bind (:map global-map
	      ("C-c i" . org-capture-inbox))

  :config
  ;; Faces for TODO keywords
  (setq org-todo-keyword-faces
	'(("PROG" . (:foreground "orange" :weight bold))
	  ("TODO" . (:foreground "#EA4E34" :weight bold))
	  ("WAIT" . (:foreground "#ff64bf" :weight bold))
	  ("CANCELLED" . (:foreground "#B50741" :weight bold))))

  ;; Block parent TODO to DONE if children are undone
  (setq org-enforce-todo-dependencies t)

  ;; Hide markup elements (default behaviour is to show)
  (setq org-hide-emphasis-markers t)

  ;; Add syntax highlighting for org documents
  ;; Also add native <Tab> behaviour in codeblocks
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t)

  ;; Org styling
  (setq org-pretty-entities nil
	org-ellipsis "…"
	org-auto-align-tags nil)

  ;; Org-indent settings
  (setq org-adapt-indentation nil)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-indent-indentation-per-level 4)

  ;; List points now use a unicode bullet symbol instead of a generic
  ;; dash or asterisk
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Org-agenda customization (based on Protesilaos Stavrou's config)
(use-package org-agenda
  :ensure nil
  ;; Don't need to go through org-agenda template
  :bind (:map global-map
	      ("C-c j" . (lambda () (interactive) (org-agenda nil "j"))))
  :config
  ;; Basic agenda setup
  (setq org-agenda-custom-commands
	'(
	  ("j" "Daily agenda and top priority tasks"
	   ((todo "PROG"
		  ((org-agenda-overriding-header "Tasks in progress")))
	    (agenda ""
		    ((org-agenda-block-separator nil)
		     (org-agenda-span 1)
		     (org-deadline-warning-days 0)
		     (org-scheduled-past-days 0)

		     ;; We don't need the `org-agenda-date-today'
		     ;; highlight because that only has a practical
		     ;; utility in multi-day views.
		     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
		     (org-agenda-format-date "%A, %-e %B %Y")
		     (org-agenda-overriding-header "\nDaily agenda")))
	    (agenda "" ((org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+1d")
			(org-agenda-format-date "%A, %-e %B %Y")
			(org-agenda-span 3)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo 'done))
			 (org-agenda-overriding-header "\nNext three days")))
	    (agenda "" ((org-agenda-time-grid nil)
			(org-agenda-start-on-weekday nil)
			(org-agenda-format-date "%A, %-e %B %Y")
			;; We don't want to replicate the previous
			;; section's three days, so we start counting
			;; from the day after.
			(org-agenda-start-day "+4d")
			(org-agenda-span 14)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:deadline))
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "\nUpcoming deadlines (+14d)")))
	    (agenda "" ((org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+1d")
			(org-agenda-format-date "%A, %-e %B %Y")
			(org-agenda-start-day "+4d")
			(org-agenda-span 14)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:scheduled))
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "\nUpcoming schedule (+14 d)")))))
	  ))

  ;; Setting org-agenda file
  ;; Eliminates the need for putting org-agenda file to the top everytime
  (setq org-agenda-files
	'("agenda.org"
	  "tasks.org"
	  "meetings.org"
	  "events.org"
	  "personal.org"
	  "schedule-europe-athens.org"
	  "birthdays.org"))

  ;; Sets TODO items to not have a prefix at the left hand side of the
  ;; org-agenda window (typically the filename where the TODO item was created).
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s")
	  (todo   . " ")
	  (tags   . " %i %-12:c")
	  (search . " %i %-12:c")))

  ;; Hides DONE items in org-agenda for schedules and deadlines
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t))

;; Org-capture templates
(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
	'(("t" "Tasks")
	  ("ts" "Scheduled tasks" entry
	   (file+headline "tasks.org" "Scheduled tasks")
	   "** TODO %?\n SCHEDULED: %^t\n")
	  ("td" "Tasks with a deadline" entry
	   (file+headline "tasks.org" "Tasks with deadline")
	   "* TODO %?\n DEADLINE: %^t\n")
	  ("w" "Wishlist" entry
	   (file+headline "tasks.org" "Wishlist")
	   "* TODO %?\n")
	  ("m" "Formal meetings")
	  ("mi" "One-to-one" entry
	   (file+headline "meetings.org" "One-to-one meetings")
	   "* MEETING with %^{With whom} at %^{Place}\n SCHEDULED: %^t")
	  ("mg" "Group" entry
	   (file+headline "meetings.org" "Group meetings")
	   "* %?\n SCHEDULED: %^t")
	  ("e" "Events" entry
	   (file+headline "events.org" "Events")
	   "* %?\n SCHEDULED: %^t")
	  ("r" "Rendez-vous")
	  ("rp" "Phone calls" entry
	   (file+headline "meetings.org" "Phone calls")
	   "* CALL with %^{With whom}%?\n SCHEDULED: %^t")
	  ("ri" "Rendezvous in-person" entry
	   (file+headline "meetings.org" "Rendezvous in-person")
	   "* HANGOUT with %^{With whom} at %^{Place}\n SCHEDULED: %^t"))))


;; Denote package by Protesilaos
;; Experimenting with it at the moment
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/org/notes/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

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
    ("M-w" . nil)
    ("C-c c" . nil)
    ("C-c a" . nil))
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
  :hook (markdown-mode . visual-line-mode)
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

;; Setup YaSnippet for LaTeX and orgmode
(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :hook ((LaTeX-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode))
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package orderless
  :custom
  ((completion-styles '(orderless basic))
  (completon-category-overrides '((file (styles basic partial-completion))))))

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
  :ensure t
  :bind (:map global-map
	      ("C-c l f" . consult-focus-lines)
	      ("C-c l g" . consult-grep)
	      ("C-c l r" . consult-ripgrep)
	      ("C-c l d" . consult-fd)
	      ("C-c l l" . consult-line)))

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

;; Stolen from Protesilaos' config
(defun pani/keyboard-quit-dwim ()
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

(define-key global-map (kbd "C-g") #'pani/keyboard-quit-dwim)

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
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure))   ;; run eglot when opening python files
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
    :map python-ts-mode-map
    ("C-l" . nil) ; unbind default binding for text view centering
    ("C-l" . xref-go-back)
    ("C-c C-n" . pani/new-python-repl)
  )
  :config

  (defun pani/new-python-repl ()
    "Start a new python interpreter in a fresh buffer."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'run-python)))

  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq python-shell-enable-font-lock nil)
  (add-hook 'eat-mode-hook #'turn-off-evil-mode nil))

;; code-cells for ipython like behaviour
(use-package code-cells
  :ensure t
  :init
  (add-hook 'python-ts-mode-hook 'code-cells-mode-maybe)
  (add-hook 'python-mode-hook 'code-cells-mode-maybe)
  :bind
  ( :map code-cells-mode-map
    ("C-c C-c" . code-cells-eval)))

;; Elfeed for RSS
(use-package elfeed
  :ensure t
  :defer t
  :hook ((elfeed-search-mode . variable-pitch-mode)
	 (elfeed-show-mode . variable-pitch-mode))
  :bind (("C-c e" . elfeed)
	 :map elfeed-search-mode-map
	 ("C-c C-c" . elfeed-update)
	 ("C-c C-v" . elfeed-arxiv-open-pdf)
	 ("C-c C-b" . (lambda () (interactive)
			(elfeed-arxiv-open-pdf (elfeed-search-selected :single) t)))
	 :map elfeed-show-mode-map
	 ("C-c C-v" . elfeed-arxiv-open-pdf)
	 ("C-c C-b" . (lambda () (interactive)
			(elfeed-arxiv-open-pdf elfeed-show-entry t))))

  :config
  ;; Wraps text to 80 characters for arxiv abstracts
  (defun pani/elfeed-fix-width-after-render (&rest _)
    "Hard wrap the buffer content to 80 chars immediately after Elfeed renders it."
    ;; Only run this in an Elfeed Show buffer
    (when (eq major-mode 'elfeed-show-mode)
      (let ((inhibit-read-only t)
	    (fill-column 90)) ;; <--- Change this number to your desired width
	(save-excursion
	  (goto-char (point-min))
	  ;; Move past the headers (Title, Date, etc.) by finding the first empty line
	  (when (search-forward "\n\n" nil t)
	    ;; Wrap the remaining text (the abstract)
	    (fill-region (point) (point-max)))))))

  ;; Attach this function to run exactly AFTER elfeed-show-refresh
  (advice-add 'elfeed-show-refresh :after #'pani/elfeed-fix-width-after-render)

  ;; Helper function: safe truncation
  (defun truncate-string-to-width-safe (str width)
    "Truncate STR to WIDTH characters if it exceeds it, adding an ellipsis."
    (if (> (string-width str) width)
	(concat (substring str 0 (max 0 (- width 1))) "…")
      str))

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
    concatenated."
    (if (> (length authors-list) 1)
	(format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

  ;; Define a custom color for the authors column
  (defface elfeed-search-author-face
    '((t :foreground "DarkOliveGreen3"))
    "Face for displaying authors in the elfeed search buffer.")

  (defun pani/my-search-print-fn (entry)
    "Print ENTRY to the buffer with dynamic column widths and right-aligned score."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
	   (title (or (elfeed-meta entry :title)
		      (elfeed-entry-title entry) ""))
	   (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	   (entry-authors (concatenate-authors
			   (elfeed-meta entry :authors)))
	   (entry-score (number-to-string (elfeed-score-scoring-get-score-from-entry entry)))
	   ;; Fixed widths
	   (date-width 12)
	   (padding 3)
	   ;; Available width for title + authors
	   (total-width (window-width))
	   (available-width (- total-width date-width padding (length entry-score))))
      ;; Clamp authors width dynamically so score always aligns
      (let* ((authors-min-width 10)
	     (authors-max-width 40)
	     (authors-width (min authors-max-width
				 (max authors-min-width (/ available-width 3))))
	     ;; Title takes remaining space
	     (title-width (max 10 (- available-width authors-width)))
	     ;; Format columns
	     (title-column (elfeed-format-column title title-width :left))
	     (authors-column (elfeed-format-column entry-authors authors-width :left))
	     ;; Right-align score by padding spaces
	     (score-padding (- total-width (+ date-width title-width authors-width padding)))
	     (score-column (concat (make-string (max 0 score-padding) ? ) entry-score)))
	;; Insert columns
	(insert (propertize date 'face 'elfeed-search-date-face) " ")
	(insert (propertize title-column 'face title-faces 'kbd-help title) " ")
	(insert (propertize authors-column 'face 'elfeed-search-author-face 'kbd-help entry-authors))
	(insert score-column))))


  (defvar elfeed--last-window-width 0
    "Stores the last known width of the Elfeed search window.")

  (defun elfeed-dynamic-resize-hook ()
    "Redisplay the Elfeed search buffer only if the window width changed."
    (when (eq major-mode 'elfeed-search-mode)
      (let ((current-width (window-width)))
	(unless (= current-width elfeed--last-window-width)
	  (setq elfeed--last-window-width current-width)
	  (let ((inhibit-read-only t))
	    (elfeed-search-update--force))))))

  ;; arxiv pdf extractor function
  (defun elfeed-arxiv-open-pdf (entry &optional open-abstract)
    "Open the arXiv PDF for the current Elfeed ENTRY in a browser.
Works in both search and show mode."
    (interactive
     (list (cond
	    ;; If in search mode, get selected entry
	    ((eq major-mode 'elfeed-search-mode)
	     (elfeed-search-selected :single))
	    ;; If in show mode, use the shown entry
	    ((eq major-mode 'elfeed-show-mode)
	     elfeed-show-entry))))
    (when entry
      (let ((link (elfeed-entry-link entry)))
	(if (string-match "arxiv\\.org/abs/\\([0-9.]+\\)" link)
	    (let ((base-id (match-string 1 link))
		  (url (if open-abstract
			   (format "https://arxiv.org/abs/%s" (match-string 1 link))
			 (format "https://arxiv.org/pdf/%s.pdf" (match-string 1 link)))))
	      (browse-url url))
	  (message "Not an arXiv link: %s" link)))))

  (add-hook 'window-configuration-change-hook #'elfeed-dynamic-resize-hook)

  (setq elfeed-search-print-entry-function #'pani/my-search-print-fn)
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq-default elfeed-search-filter "@6-months-ago +arxiv")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/downloads/")
  (setq elfeed-search-date-format '("%Y-%m-%d" 0 :left))
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 90)
  (setq elfeed-search-trailing-width 0)
  (setq elfeed-feeds
	'(("http://export.arxiv.org/api/query?search_query=cat:quant-ph+cond-mat.mes-hall&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending"
	   arxiv)
	  ("http://emacsredux.com/atom.xml" emacs)
	  ("http://lambdaland.org/index.xml" emacs)
	  ("http://www.emacs.dyerdwelling.family/index.xml" emacs)
	  ("http://sachachua.com/blog/category/emacs/feed/index.xml"
	   emacs)
	  ("http://karthinks.com/index.xml" emacs)
	  ("https://feyor.sh/atom.xml" emacs blog)
	  ("http://ogbe.net/blog-feed.xml" emacs blog)
	  ("http://protesilaos.com/codelog.xml" emacs)
	  ("http://themkat.net/feed.xml" emacs)
	  ("https://coredumped.dev/index.xml" emacs)
	  ("http://irreal.org/blog/?tag=emacs&feed=rss2" emacs)
	  ("http://marci.gunyho.com/rss" blog)
	  ("http://11de784a.github.io/feed.xml" blog)
	  ("http://terrytao.wordpress.com/feed/" blog)
	  ("http://m-malinowski.github.io/feed.xml" blog)
	  ("http://timharek.no/rss.xml" blog)
	  ("http://matt.might.net/articles/feed.rss" blog)
	  ("http://protesilaos.com/commentary.xml" blog)
	  ("http://gregorygundersen.com/feed.xml" blog)
	  ("http://rosenzweig.io/feed.xml" blog)
	  ("http://adol.pw/index.xml" blog)
	  ("http://tony-zorman.com/atom.xml" blog)
	  ("http://andreyor.st/feed.xml" blog)
	  ("http://www.paritybit.ca/feed.xml" blog)
	  ("http://kishvanchee.com/index.xml" blog)
	  ("http://dataswamp.org/~solene/rss.xml" blog)
	  ("http://drewdevault.com/blog/index.xml" blog)
	  ("http://simon.peytonjones.org/feed.xml" blog)
	  ("http://jvns.ca/atom.xml" blog)
	  ("http://100r.co/links/rss.xml" blog)
	  ("http://anchor.fm/s/c761a04/podcast/rss" blog)
	  ("http://www.romanzolotarev.com/rss.xml" tech)
	  ("http://www.unixsheikh.com/feed.rss" tech)
	  ("http://nullprogram.com/feed/" tech)
	  ("http://yarmo.eu/atom.xml" tech)
	  ("http://bernsteinbear.com/feed.xml" tech)
	  ("http://changelog.complete.org/feed" tech)
	  ("http://phdcomics.com/gradfeed.php" comics)
	  ("http://www.noendcomic.com/rss.php" comics)
	  ("http://xkcd.com/atom.xml" comics)
	  ("http://www.smbc-comics.com/comic/rss" comics)
	  ("http://www.commitstrip.com/en/feed/" comics))))

(use-package elfeed-score
  :ensure t
  :after elfeed
  :config
  (setq elfeed-score-rule-stats-file "~/.emacs.d/elfeed.stats")
  (setq elfeed-score-serde-score-file "~/.emacs.d/elfeed.score")
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

;; Olivetti for reducing eye strain
(use-package olivetti
  :ensure t
  :hook
  ((elfeed-search-mode . olivetti-mode)
   (elfeed-show-mode . olivetti-mode))
  :config
  (setq olivetti-minimum-body-width 180)
  (setq olivetti-body-width 100))

;; Hide autorevert mode in modeline
(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode 1))

;; Info-mode config
(use-package info
  :ensure nil ; built-in
  :bind
  (:map Info-mode-map
	("<SPC>" . nil))  ;; TODO: Fix this binding as its bound to `Info-scroll-up'
  :hook (Info-mode . variable-pitch-mode))

;; TRAMP config
(use-package tramp
  :ensure nil ; built-in
  :config
  (setq tramp-verbose 10)
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp)))

;; Treesitter config
(use-package treesit
  :ensure nil ; built-in
  :config
  ;; Remap major modes to treesitter
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode)))
  ;; Tree-sitter font lock (4 is highest)
  (setq treesit-font-lock-level 4))

;; Typst support
(use-package typst-ts-mode
  :defer t
  :config
  ;; Disable formatting for superscripts and subscripts
  (setq typst-ts-math-script-display '((raise 0.0) raise 0.0))
  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode)))

;; IRC inside emacs
(use-package erc
  :ensure nil
  :config
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
	erc-server "irc.libera.chat"
	erc-nick "paniash"
	erc-user-full-name "Ashish Panigrahi"
	erc-auto-query 'bury
	erc-fill-column 100
	erc-fill-function 'erc-fill-static
	erc-fill-static-center 20))


;; PDF support inside emacs
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Initialize the server
  (pdf-tools-install :no-query)

  ;; Tweaks for better rendering
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick t)

  :bind (:map pdf-view-mode-map
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("C-s" . isearch-forward)))

;; Smooth scrolling
(use-package pixel-scroll
  :ensure nil
  :bind (:map global-map
	      ([remap scroll-up-command] . pixel-scroll-interpolate-down)
	      ([remap scroll-down-command] . pixel-scroll-interpolate-up))
  :custom
  (pixel-scroll-precision-interpolate page t)
  (pixel-scroll-precision-mode t)
  :init
  (pixel-scroll-precision-mode 1))
