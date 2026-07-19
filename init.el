;;; -*- lexical-binding: t; -*-
;; Initialize package sources
(use-package package
  :ensure nil
  :config
  (setq use-package-always-ensure nil)
  (setq package-install-upgrade-built-in nil)
  (setq package-archives '(("melpa" . "https://snapshots.melpa.org/packages/")
			   ("non-gnu" . "https://elpa.nongnu.org/nongnu/")
			   ("elpa" . "https://elpa.gnu.org/packages/")))
  (setq package-archive-priorities
	'(("gnu-elpa" . 3)
	  ("nongnu" . 2)
	  ("melpa" . 1)))
  ; check package-review-policy only if this variable exists (available only after emacs 31)
  (when (boundp 'package-review-policy)
    (setq package-review-policy nil  ; very manual at this point (needs automation)
	  package-review-diff-command '("git" "diff" "--no-index"
					"--color=never"
					"--diff-filter=d")))
  ;; Initialize all packages after package.el is loaded
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

;;; Some standard emacs config
(use-package emacs
  :ensure nil
  :demand t
  :init
  ;; Remove visible bell
  (setq visible-bell nil
        ring-bell-function #'ignore)

  ;; Enable imenu support for use-package declarations
  (setopt use-package-enable-imenu-support t)

  (setq mode-line-right-align-edge 'right-margin)

  ;; Buffer list configuration
  (setq display-buffer-alist
	'(
	  ;; Python buffers now occupy 25% of the screen
	  ("\\*Python\\*"
	   (display-buffer-reuse-window
	    display-buffer-at-bottom)
	   (dedicated . t)
	   (window-height . 0.25))

	  ;; Silence byte compile warnings when installing new packages
	  ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	   (display-buffer-no-window)
	   (allow-no-window . t))

	  ;; Compilation mode buffers occupy small fraction of the screen
	  ((derived-mode . compilation-mode)
	   (display-buffer-reuse-window
	    display-buffer-at-bottom)
	   (dedicated . t)
	   (window-height . 0.15))

	  ;; Typst ts compilation mode settings
	  ((derived-mode . typst-ts-compilation-mode)
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (window-height . fit-window-to-buffer))

	  ;; vc-compilation mode settings
	  ((derived-mode . vc-compilation-mode)
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (window-height . fit-window-to-buffer))

	  ;; help-mode settings
	  ((derived-mode . help-mode)
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (window-height . (lambda (window)
			      (fit-window-to-buffer window (floor (* 0.5 (frame-height)))))))

	  ;; ibuffer settings
	  ((derived-mode . Buffer-menu-mode)
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (body-function . (lambda (window) (select-window window)))
	   (window-height . (lambda (window)
			      (fit-window-to-buffer window (floor (* 0.5 (frame-height)))))))

	  ;; Eldoc specific settings
	  ("\\*eldoc\\*"
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (window-height . fit-window-to-buffer))

	  ;; xref buffer specific settings
	  ("\\*xref\\*"
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   (window-height . fit-window-to-buffer))

	  ;; Occur specific buffer settings
	  ("\\*Occur\\*"
	   ;; If a buffer with the matching major-mode exists in
	   ;; some window, then use that one. Otherwise, display
	   ;; the buffer below the current window.
	   (display-buffer-reuse-mode-window
	    display-buffer-pop-up-window)
	   ;; Then we have some parameters
	   (dedicated . t)
	   (body-function . (lambda (window) (select-window window)))
	   (window-width . 0.5)
	   (window-height . (lambda (window)
			      (fit-window-to-buffer window (floor (* 0.65 (frame-height)))))))))

  ;; Fonts
  (set-face-attribute 'default nil :font "Hack" :height 115)
  (set-face-attribute 'fixed-pitch nil :font "Hack" :height 1.0)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Term" :height 1.05)

  ;; Personal details
  (setopt user-full-name "Ashish Panigrahi")
  (setopt user-mail-address "public@ashishpanigrahi.com")

  ;; Always load newest byte code
  (setopt load-prefer-newer t)

  ;; Display load message when starting emacs
  (defun efs/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
	     (format "%.3f seconds"
		     (float-time
		      (time-subtract after-init-time before-init-time)))
	     gcs-done))

  (add-hook 'emacs-startup-hook #'efs/display-startup-time)

  ;; Stolen from Protesilaos' config + my own for consult-focus-lines quitting
  (defun pani/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- When lines are hidden with `consult-focus-lines', reveal them.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     ((and (boundp 'consult--focus-lines-overlays)
	   consult--focus-lines-overlays)
      (consult-focus-lines nil t))
     (t
      (keyboard-quit))))

  :bind
  ("C-g" . pani/keyboard-quit-dwim)

  :config
  (setq kill-do-not-save-duplicates t)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq tab-always-indent 'complete) ; tab for completion and not indenting
  (setq auto-save-timeout nil)
  (setq help-window-select t) ; Cursor focus goes to help window when invoked
  (setq help-window-keep-selected t)  ; Keep using the same window for more help buffers
  (setq compilation-scroll-output t) ; scroll compilation buffer as output appears

  (column-number-mode 1)
  (global-visual-wrap-prefix-mode 1)

  ;; Have prose modes auto wrap long lines
  (dolist (hook '(org-mode-hook markdown-mode-hook markdown-ts-mode-hook LaTeX-mode-hook))
    (add-hook hook #'auto-fill-mode))

  ;; Deletes trailing whitespace upon saving a file
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Vim like scrolling
  (setq scroll-step            1
	scroll-conservatively  10000
	next-screen-context-lines 5
	;; move by logical lines rather than visual lines (better for macros)
	line-move-visual nil)

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
   '(overwrite-mode iconify-frame diary))

  ;; Hide minor modes in the modeline
  (when (boundp 'mode-line-collapse-minor-modes)
    (setq mode-line-collapse-minor-modes t))

  ;; No ugly buttons for checkboxes
  (setq widget-image-enable nil)

  ;; Do not use graphical dialogs
  (setq use-dialog-box nil)

  ;; Enable tooltips (default)
  (tooltip-mode 1)

  ;; Disable remote file locks
  (setq remote-file-name-inhibit-locks t)

  ;; Put autosave files in one folder
  (setq backup-directory-alist `(("." . "~/.autosaves")))
  (setq backup-inhibited t)
  (setq auto-save-default nil)

  ;; Replace "yes or no" with "y or n"
  (setq use-short-answers t)

  ;; ;; Enable `completion-preview-mode' for certain hooks
  ;; :hook (python-mode . completion-preview-mode)

  (setq display-line-numbers-type 'relative)

  ;; Disable bidirectional text scanning
  (setq-default bidi-display-reordering 'left-to-right
		bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)

  ;; Skip fontification during input
  (setq redisplay-skip-fontification-on-input t)

  ;; Increase process output buffer for LSP
  ;; Basically free performance with eglot
  (setq read-process-output-max (* 4 1024 1024)) ; 4 MB

  ;; Prevent ffap from pinging hostnames
  (setq ffap-machine-p-known 'reject)

  ;; Proportional window resizing (becomes apparent with multiple windows
  (setq window-combination-resize t)

  ;; Enable line numbers only for programming and text modes
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)

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

  ;; Auto-chmod scripts on save
  (add-hook 'after-save-hook
	    #'executable-make-buffer-file-executable-if-script-p)

  :bind
  ( :map global-map
    ("C-x C-d" . nil) ; never use it
    ("C-x C-z" . nil) ; never use it
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-h h" . nil) ; never show that "hello" file
    ("C-l" . nil) ; never use it
    ("C-k" . nil) ; never use it
    ("C-x <left>" . nil) ; unbind the `previous-buffer' command
    ("M-o" . other-window) ; switch between windows
    ("C-x C-p" . mode-line-other-buffer)  ; switches between two buffers
    ("C-x w t" . window-layout-transpose) ; switch between vertical and horizontal layout
    )
  :config
  ;; disable Ispell completion function
  (setq text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not work in current buffer
  (setq read-extended-command-predicate
	#'command-completion-default-include-p))

(use-package shr
  :ensure nil
  :config
  (setq shr-use-colors nil)
  (setq shr-ignore-cache t)
  ;; Disable html font size overriding default fonts
  (setq shr-use-fonts nil))

;;; Vim Bindings
(use-package evil
  :ensure t
  :demand t
  :bind (:map global-map
	      ("<escape>" . keyboard-escape-quit))
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
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up) ; explicitly bind C-u
  (define-key evil-motion-state-map (kbd "C-o") 'occur) ; `occur' now bound to C-o
  (define-key evil-motion-state-map (kbd "C-q") 'query-replace) ; `query-replace' now bound to C-q

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Setting leader key in emacs
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key nil 'global
    (kbd "<leader>b") 'consult-buffer)
  (evil-define-key nil 'global
    (kbd "<leader>v") 'consult-project-buffer)

  ;; Set `t' as a prefix key for tab manipulation commands
  (define-prefix-command 'pani/t-key)
  (define-key evil-normal-state-map (kbd "t") 'pani/t-key)
  (define-key pani/t-key (kbd "j") 'tab-previous)
  (define-key pani/t-key (kbd "k") 'tab-next)
  (define-key pani/t-key (kbd "n") 'tab-new)
  (define-key pani/t-key (kbd "x") 'tab-close)
  (define-key pani/t-key (kbd "X") 'tab-close-other)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'inferior-python-mode 'emacs))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :vc (:url "https://github.com/emacs-evil/evil-collection"
	    :rev :newest)
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init)
  ;; Vim-commentary without any extra package
  (with-eval-after-load "evil"
    (evil-define-operator my-evil-comment-or-uncomment (beg end)
      "Toggle comment for the region between BEG and END."
      (interactive "<r>")
      (comment-or-uncomment-region beg end))
    (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment)))

;; Some useful config for emacs-lisp
(use-package emacs-lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-r" . eval-region)
	      ("C-c C-b" . eval-buffer)))

(use-package undo-fu
  :ensure t
  :demand t
  :commands (undo-fu-only-undo
	     undo-fu-only-redo
	     undo-fu-only-redo-all
	     undo-fu-disable-checkpoint)
  :config
  ;; 3 times the default values
  (setq undo-limit (* 3 160000))
  (setq undo-strong-limit (* 3 240000)))

;; Protesilaos' ef-themes
(use-package ef-themes
  :vc (:url "https://github.com/protesilaos/ef-themes"
       :rev :newest)
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
	  (agenda-date variable-pitch 1.1)
	  (agenda-structure variable-pitch light 1.4)
	  (t variable-pitch 1.1)))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-symbiosis :no-confirm)
  (set-face-attribute 'minibuffer-prompt nil :weight 'normal))

;;; Vertico
(use-package vertico
  :ensure t
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

  ;; Fix font for vertico group title
  (set-face-attribute 'vertico-group-title nil
		      :height 1.0
		      :slant 'normal
		      :weight 'bold)

  ;; This removes the shadowed-out region when finding file from an
  ;; already arrived directory
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; Which-key (to show available commands when typing a prefix say 'C-c')
(use-package which-key
  :ensure nil  ; built-in
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Image settings inside emacs
(use-package image
  :ensure nil
  :config
  (defun pani/png-white-bg (orig file-or-data &optional type data-p &rest props)
    (when (or (eq type 'png)
              (and (stringp file-or-data) (not data-p)
                   (string-match-p "\\.png\\'" file-or-data)))
      (setq props (plist-put props :background "white")))
    (apply orig file-or-data type data-p props))
  (advice-add 'create-image :around #'pani/png-white-bg)

  (defun pani/image--mime-type (image)
    "Return the MIME type of IMAGE, sniffing its payload where possible.
The :type property can be `imagemagick' or otherwise unhelpful, so prefer
detection from the actual bytes."
    (let* ((data (image-property image :data))
	   (file (image-property image :file))
	   (type (or (and data (image-type-from-data data))
		     (and file (image-type-from-file-header file))
		     (image-property image :type))))
      (pcase type
	('jpeg "image/jpeg")
	('gif  "image/gif")
	('tiff "image/tiff")
	('webp "image/webp")
	('svg  "image/svg+xml")
	(_     "image/png"))))

  (defun pani/image--payload-file (image)
    "Return a file holding IMAGE's bytes, plus non-nil if it's a temp file.
Images rendered by shr carry their payload in :data, so write it out;
file-backed images are used in place."
    (let ((data (image-property image :data))
          (file (image-property image :file)))
      (cond
       (data (let ((tmp (make-temp-file "emacs-image-"))
                   (coding-system-for-write 'binary))
               (with-temp-file tmp
                 (set-buffer-multibyte nil)
                 (insert data))
               (cons tmp t)))
       (file (cons (expand-file-name file) nil))
       (t (user-error "Image at point has no payload")))))

  (defun pani/image-copy-to-clipboard ()
    "Copy the image at point to the X clipboard via `xclip'.
Emacs cannot own a non-text X selection, so xclip is spawned detached and
lingers as the selection owner."
    (interactive)
    (unless (executable-find "xclip")
      (user-error "`xclip' not found"))
    (let* ((image (image--get-image))
           (mime  (pani/image--mime-type image))
           (payload (pani/image--payload-file image))
           (file  (car payload))
           (tempp (cdr payload)))
      ;; Pass the file as INFILE: Emacs opens it and hands the fd to the child
      ;; before forking, so unlinking it afterwards is safe.  DESTINATION 0
      ;; means "don't wait" -- xclip must stay alive to serve the selection.
      (call-process "xclip" file 0 nil "-selection" "clipboard" "-t" mime)
      (when tempp
        (ignore-errors (delete-file file)))
      (message "Copied image (%s) to clipboard" mime)))

  ;; `i o' is `image-save'; give it a sibling.  `i c' is taken by `image-crop'.
  (keymap-set image-map "i w" #'pani/image-copy-to-clipboard))

;; Orgmode specific settings
(use-package org
  :ensure nil  ; org is built-in
  :defer t
  :init
  (setq org-directory (expand-file-name "~/org/"))
  (setq org-imenu-depth 7)
  :hook (org-mode . auto-revert-mode)
  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      ("C-c c" . org-capture)
	 :map org-mode-map
	 ("C-x a" . org-archive-subtree-default)
	 ("C-x i" . org-toggle-inline-images)
	 ("C-c C-p" . org-priority)
	 ("C-c C-j" . nil))  ;; I don't use `org-goto'
  :config
  (setq org-log-done 'time)
  ;; Collapse the log entries into a "drawer"
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; org-indent-mode turned on by default
  (setq org-startup-indented t)
  ;; Only show content and keep rest folded
  (setq org-startup-folded 'content)
  ;; Emacs identifies sentences with a single space after fullstop.
  (setq sentence-end-double-space nil)
  ;; Start calendar week from Monday
  (setq calendar-week-start-day 1)

  ;; Unhides leading stars (default behaviour)
  (setq org-hide-leading-stars nil)

  ;; Turn on cdlatex minor mode in all org buffers
  ;; See https://orgmode.org/manual/CDLaTeX-mode.html for details
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  ;; Set renderer for LaTeX preview in orgmode
  (setq org-preview-latex-default-process 'imagemagick)

  ;; Faces for TODO keywords
  (setq org-todo-keyword-faces
	'(("PROG" . (:foreground "orange" :weight bold))
	  ("TODO" . (:foreground "#EA4E34" :weight bold))
	  ("WAIT" . (:foreground "#8E97ED" :weight bold))
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
	org-auto-align-tags t)

  ;; Org-indent settings
  (setq org-adapt-indentation nil)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-indent-indentation-per-level 4)

  ;; Org-babel settings
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)))

  (setq org-babel-python-command python-shell-interpreter)

  ;; Don't ask me everytime I evaluate a src-block (only for python and elisp)
  (setq org-confirm-babel-evaluate
	(lambda (lang _body)
	  (not (member lang '("python" "emacs-lisp")))))

  ;; List points now use a unicode bullet symbol instead of a generic
  ;; dash or asterisk
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region
					   (match-beginning 1)
					   (match-end 1) "•"))))))

  ;; Function to open a zotero link inside zotero as a pdf viewer
  (defun org-zotero-open-link (path _)
    (call-process "xdg-open" nil nil nil (concat "zotero:" path)))

  (org-link-set-parameters "zotero" :follow #'org-zotero-open-link))

;; Org-agenda customization (based on Protesilaos Stavrou's config)
(use-package org-agenda
  :ensure nil
  :hook ((org-agenda-mode . pani/org-agenda-font-size)
	 (org-agenda-finalize . pani/org-agenda-birthday-emoji))
  ;; Don't need to go through org-agenda template for custom agenda
  :bind (:map global-map
	      ("C-c j" . pani/custom-org-agenda)
	      :map org-agenda-mode-map
	      ("C-c C-p" . org-agenda-priority))
  :config
  ;; Custom function to resize fonts in org-agenda
  (defun pani/org-agenda-font-size ()
    "Render org-agenda in variable-pitch at ~1.17x the global default height.
Uses a single `face-remap-set-base' so it overwrites rather than stacks,
keeping the size stable across `g'/`org-agenda-redo'."
    (let ((global-height (face-attribute 'default :height nil 'default)))
      (face-remap-set-base 'default
			   (list :inherit 'variable-pitch
				 :height (round (* global-height 1.17))))))

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
		     (org-scheduled-past-days 2)

		     ;; We don't need the `org-agenda-date-today'
		     ;; highlight because that only has a practical
		     ;; utility in multi-day views.
		     ;; Also, if TODAY is a weekend, display font-face `org-agenda-date-weekend'
		     ;; else display `org-agenda-date'.
		     (org-agenda-day-face-function
		      (lambda (date)
			(if (member (calendar-day-of-week date) org-agenda-weekend-days)
			    'org-agenda-date-weekend
			  'org-agenda-date)))
		     (org-agenda-skip-function
		      '(org-agenda-skip-entry-if 'todo '("PROG")))
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
			(org-agenda-format-date "%A, %-e %B %Y")
			(org-agenda-start-day "+4d")
			(org-agenda-span 14)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-block-separator nil)
			(org-agenda-entry-types '(:scheduled :timestamp :sexp))
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "\nUpcoming schedule (+14d)")))))
	  ))

  (defun pani/custom-org-agenda ()
    "Custom function to immediately jump to my custom org-agenda view."
    (interactive)
    (org-agenda nil "j"))

  (defun pani/org-agenda-birthday-emoji ()
    "Append a cake emoji for birthday entries."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (equal (org-get-at-bol 'org-category) "birthdays")
	  (end-of-line)
	  (insert " 🎂"))
	(forward-line 1))))

  ;; Setting org-agenda file
  ;; Eliminates the need for putting org-agenda file to the top everytime
  (setq org-agenda-files
	'("agenda.org"
	  "tasks.org"
	  "meetings.org"
	  "events.org"
	  "personal.org"
	  "calendar.org"))

  ;; Sets TODO items to not have a prefix at the left hand side of the
  ;; org-agenda window (typically the filename where the TODO item was created).
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s")
	  (todo   . " ")
	  (tags   . " %i %-12:c")
	  (search . " %i %-12:c")))

  ;; Don't run emacs diary when invoking org-agenda
  (setq org-agenda-include-diary nil)

  ;; User-defined colors for custom org-agenda
  (custom-set-faces
   '(org-agenda-structure ((t (:foreground "#689f38" :weight normal))))
   '(org-agenda-date ((t (:foreground "#dc8add" :weight normal))))
   '(org-agenda-date-weekend ((t (:foreground "#3f95f6" :weight normal))))
   '(org-agenda-calendar-event ((t (:foreground "#a8a8a8" :weight normal))))
   '(org-agenda-calendar-daterange ((t (:foreground "#a8a8a8" :weight normal))))
   '(org-scheduled ((t (:foreground "#84a8a4" :weight normal))))
   '(org-scheduled-today ((t (:foreground "#84a8a4" :weight normal))))
   '(org-scheduled-previously ((t (:foreground "#7986cb" :weight normal))))
   '(org-deadline-today ((t (:foreground "#f7768e" :weight normal))))
   '(org-tag ((t (:foreground "#56b6c2" :weight light))))
   )

  ;; Change text for past scheduled items
  (setq org-agenda-scheduled-leaders '("Scheduled: " "Late%2dd:   "))

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
	   "* TODO [#%^{Priority|B|A|C}] %^{Task} %^g\n SCHEDULED: %^t\n" :empty-lines-after 1)
	  ("td" "Tasks with a deadline" entry
	   (file+headline "tasks.org" "Tasks with deadline")
	   "* TODO [#%^{Priority|B|A|C}] %^{Task} %^g\n DEADLINE: %^T\n" :empty-lines-after 1)
	  ("w" "Wishlist" entry
	   (file+headline "tasks.org" "Wishlist")
	   "* TODO %^{Task}%? %^g\n" :empty-lines-after 1)
	  ("m" "Meetings")
	  ("mi" "One-to-one" entry
	   (file+headline "meetings.org" "One-to-one meetings")
	   "* MEETING with %^{With whom} at %^{Place} %^g :discussion:\n %^T" :empty-lines-after 1)
	  ("mg" "Group" entry
	   (file+headline "meetings.org" "Group meetings")
	   "* %^{Event} MEETING%? %^g :meeting:\n %^T" :empty-lines-after 1)
	  ("e" "Events" entry
	   (file+headline "events.org" "Events")
	   "* %^{Event}%? %^g\n %^t" :empty-lines-after 1)
	  ("r" "Rendez-vous")
	  ("rp" "Phone calls" entry
	   (file+headline "meetings.org" "Phone calls")
	   "* CALL with %^{With whom}%? :social:\n %^T" :empty-lines-after 1)
	  ("ri" "Rendezvous in-person" entry
	   (file+headline "meetings.org" "Rendezvous in-person")
	   "* HANGOUT with %^{With whom} at %^{Place} :social:\n %^T" :empty-lines-after 1))))


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

;; Magit
(use-package magit
  :ensure t
  :bind
  ( :map global-map
    ("C-x g" . magit-status)
    ("M-c" . magit-clone)
    :map magit-mode-map
    ("C-w" . nil)
    ("M-w" . nil)
    ("C-c c" . nil)
    ("C-c a" . nil))
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq git-commit-summary-max-length 70)
  ;; updates vc branch info in modeline instantaneously
  ;; probably not very good for the CPU
  (setq auto-revert-check-vc-info t)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-diff-refine-hunk 'all))

(use-package markdown-mode
  :ensure t
  :mode ("*.md" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-ts-mode
   :mode ("\\.md\\'" . markdown-ts-mode)
   :ensure nil
   :defer t)

;; Marginalia package
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; TeX config
(use-package tex-site
  :ensure auctex
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

(use-package tex-site
  :ensure cdlatex)

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic
						     partial-completion))))
  (setq completion-pcm-leading-wildcard t))

;; World-clock customization
;;;; World clock (M-x world-clock)
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("UTC" "UTC")
          ("Europe/Berlin" "Aachen")
          ("Europe/Helsinki" "Helsinki")
          ("Asia/Kolkata" "Chennai")
          ("Asia/Singapore" "Singapore")
          ("Asia/Tokyo" "Tokyo")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-sort-order "%FT%T")
  (setq world-clock-timer-second 60))

(use-package consult
  :ensure t
  :bind (:map global-map
	      ("C-c l f" . consult-focus-lines)
	      ("C-c l g" . consult-grep)
	      ("C-c l r" . consult-ripgrep)
	      ("C-c l d" . consult-fd)
	      ("C-c l i" . consult-imenu)
	      ("C-c l l" . consult-line)

	      :map org-mode-map
	      ("C-c l o" . consult-org-heading)
	      ("C-c l a" . consult-org-agenda)))

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
  :hook ((dired-mode . dired-hide-details-mode)
	 (dired-mode . hl-line-mode))
  :bind (:map global-map
	      ("C-c C-d" . pani/fd-jump)
	 :map vertico-map
	      ("C-c C-d" . pani/fd-jump))
  :init
  (defun pani/fd-executable ()
    "Return the local fd binary (`fd' or `fdfind')."
    (let ((default-directory "~/"))   ; force a local lookup, not over TRAMP
      (or (executable-find "fd")
	  (executable-find "fdfind")
	  (user-error "Neither `fd' nor `fdfind' found"))))

  ;; Custom function to mimick fzf+fd fuzzy-style `cd'
  (defun pani/fd-jump ()
    "Fuzzy-jump into a directory below a root via fd.

When called from a file-name minibuffer (an unconfirmed `C-x C-f' prompt),
replace the path being edited with the chosen directory, so you keep typing
or completing a filename there.  Otherwise, open dired at the chosen directory."
    (interactive)
    (let* ((in-file-mb (pani/fd--file-name-mb-p))
	   (root (pani/fd--root))
	   (default-directory root)
	   (dirs (process-lines (pani/fd-executable) "--type" "directory" "--strip-cwd-prefix"
				"--max-depth" "3"))
	   (target (completing-read
		    (format "Directory (under %s): " (abbreviate-file-name root))
		    dirs nil t))
	   (abs (file-name-as-directory (expand-file-name target root))))
      (if in-file-mb
	  (progn
	    (delete-minibuffer-contents)
	    (insert abs))
	(dired abs))))

  (defun pani/fd--file-name-mb-p ()
    "Non-nil if the active minibuffer is reading a file name."
    (and (minibufferp)
	 (bound-and-true-p minibuffer-completing-file-name)))

  (defun pani/fd--root ()
    "Return the fd search root.
In a file-name minibuffer, use the directory of the path being edited;
otherwise use the current buffer's `default-directory'."
    (if (pani/fd--file-name-mb-p)
	(let* ((expanded (expand-file-name (minibuffer-contents-no-properties)))
	       (dir (if (directory-name-p expanded)
			expanded
		      (file-name-directory expanded))))
	  (or (and dir (file-directory-p dir) (file-name-as-directory dir))
	      default-directory))
      default-directory))

  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-isearch-filenames 'dwim)
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
    ("C-v" . yank))
  :config
  (setq enable-recursive-minibuffers t)
  (setq minibuffer-depth-indicate-mode 1))

;; Mailcap settings
(use-package mailcap
  :ensure nil
  :config
  ;; Prefer librewolf first, otherwise firefox for opening pdfs
  (defvar pani/pdf-browser
    (or (seq-find #'executable-find '("librewolf" "firefox")) "firefox"))

  (add-to-list 'mailcap-user-mime-data
	       `((viewer . ,(concat pani/pdf-browser " %s"))
		 (type . "application/pdf"))))

(use-package emacs
  :ensure nil
  :config
  (defvar notmuch-path
    (if (string=(system-name) "d22-0153")
	"/usr/share/emacs/site-lisp/elpa/notmuch-0.38.3/"
      "/usr/share/emacs/site-lisp/")))

;; Notmuch for email
(use-package notmuch
  :load-path notmuch-path
  :ensure nil
  :defer t
  :commands (notmuch notmuch-mua-new-mail)
  :hook ((notmuch-hello-mode . variable-pitch-mode)
	 (notmuch-search-mode . variable-pitch-mode)
	 (notmuch-tree-mode . variable-pitch-mode)
	 (notmuch-show-mode . variable-pitch-mode))
  :init
  ;; Search
  (setq notmuch-search-oldest-first nil)
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
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-40s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-unthreaded-result-format
        '(("date" . "%12s  ")
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
  (setq notmuch-saved-searches
	(let ((common '((:name "inbox" :query "tag:inbox" :key "i")
			(:name "unread" :query "tag:unread" :key "u")
			(:name "drafts" :query "tag:draft" :key "d")
			(:name "all email" :query "*" :key "A")))
	      (personal '((:name "aalto-inbox" :query "tag:aalto and not tag:sent" :key "aa")
			  (:name "aalto-sent" :query "tag:aalto and tag:sent" :key "as")
			  (:name "migadu-inbox" :query "tag:migadu and not tag:sent" :key "mm")
			  (:name "migadu-sent" :query "tag:migadu and tag:sent" :key "ms")
			  (:name "niser" :query "tag:niser" :key "n")
			  (:name "juelich" :query "tag:juelich" :key "j")
			  (:name "emacs-orgmode" :query "tag:orgmode" :key "o"))))
	  (if (string= (system-name) "d22-0153")
	      common
	    (append common personal))))

  ; Tags
  (setq notmuch-archive-tags nil ; I don't archive email
	notmuch-message-replied-tags '("+replied" "-unread")
	notmuch-message-forwarded-tags '("+forwarded" "-unread")
	notmuch-show-mark-read-tags '("-unread")
	notmuch-draft-tags '("+draft" "-unread")
	notmuch-draft-folder "university/Drafts"
	notmuch-fcc-dirs '(("ashish\\.panigrahi@aalto\\.fi" . "university/Sent -inbox -unread +sent +aalto")
			   ("@ashishpanigrahi\\.com" . "migadu/Sent -inbox -unread +sent +migadu"))
	notmuch-draft-save-plaintext 'ask)

  ; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal)
  (setq notmuch-address-use-company nil)
  (if (string= (system-name) "d22-0153")
      (setq notmuch-always-prompt-for-sender nil)
    (setq notmuch-always-prompt-for-sender t))
  (setq notmuch-mua-cite-function
	'message-cite-original-without-signature)
  (setq notmuch-mua-user-agent-function nil)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)

  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block html images
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  (setq notmuch-identities
	'("Panigrahi Ashish <ashish.panigrahi@aalto.fi>"
	  "Ashish Panigrahi <public@ashishpanigrahi.com>"
	  "Ashish Panigrahi <ashish@ashishpanigrahi.com>"))

  ;; Prot customization
  (let ((count 5)) ; Show only 5 lines by default for quoted text
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))

  ;; Increase variable-pitch font (similar to elfeed setup)
  (defun pani/notmuch-variable-pitch ()
    "Render notmuch buffers in variable-pitch, scaled 1.15x."
    (face-remap-add-relative 'default '(:inherit variable-pitch :height 1.15))
    (face-remap-add-relative 'fixed-pitch 'variable-pitch))

  (define-advice notmuch-read-query (:around (orig-fun &rest args) pani/notmuch-vertico)
    "Make `notmuch-read-query' use `completing-read' so Vertico drives completion.
notmuch reads its query with `read-from-minibuffer' and binds TAB to
`minibuffer-complete'; Vertico only advises `completing-read', so it never
engages and you get the default *Completions* buffer.  Intercept notmuch's
`read-from-minibuffer' call and hand off to `completing-read' (reusing
notmuch's own dynamic table), restoring the real `read-from-minibuffer' around
that call so `completing-read' doesn't recurse back into us.  SPC self-inserts
in `vertico-map', so multi-term queries still work."
    (let ((real-rfm (symbol-function 'read-from-minibuffer)))
      (cl-letf (((symbol-function 'read-from-minibuffer)
		 (lambda (prompt &optional initial-contents _keymap _read hist default &rest _)
		   (cl-letf (((symbol-function 'read-from-minibuffer) real-rfm))
		     (completing-read prompt minibuffer-completion-table
				      nil nil initial-contents hist default)))))
	(apply orig-fun args))))

  ;; Stolen from Prot's config
  (defun pani/notmuch-message-tab ()
    "Override for `message-tab' to enforce header line check.
More specifically, perform address completion when on a relevant header
line, because `message-tab' sometimes (not sure when/how) fails to do
that and instead tries to complete against dictionary entries."
    (interactive nil message-mode)
    (cond
     ((save-excursion
	(goto-char (line-beginning-position))
	(looking-at notmuch-address-completion-headers-regexp))
      (notmuch-address-expand-name)
      ;; Completion was performed; nothing else to do.
      nil)
     (message-tab-body-function (funcall message-tab-body-function))
     (t (funcall (or (lookup-key text-mode-map "\t")
		     (lookup-key global-map "\t")
		     'indent-relative)))))

  (advice-add #'message-tab :override #'pani/notmuch-message-tab)

  ;; Decryption settings
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-crypto-gpg-program 'gpg2)

  (defun pani/notmuch-jump-inbox ()
    "Jump straight to notmuch inbox."
    (interactive)
    (notmuch-search "tag:inbox"))

  (defun pani/notmuch-jump-unread ()
    "Jump straight to notmuch unread tags."
    (interactive)
    (notmuch-search "tag:unread"))

  (defun pani/notmuch-jump-today ()
    "Jump straight to notmuch unread tags."
    (interactive)
    (notmuch-search "date:today"))

  (defun pani/notmuch-jump-migadu ()
    "Jump straight to personal (migadu) inbox."
    (interactive)
    (notmuch-search "tag:migadu"))

  (defun pani/notmuch-jump-orgmode ()
    "Jump straight to orgmode mailing list inbox."
    (interactive)
    (notmuch-search "tag:orgmode"))

  (defun pani/notmuch-mua-empty-subject-check ()
    "Prompt for confirmation before sending a message with empty subject."
    (when (and (null (message-field-value "Subject"))
	       (not (y-or-n-p "Subject is empty, send anyway? ")))
      (error "Sending message cancelled: empty subject.")))

  (defun pani/notmuch-show-ret ()
    "Expand a wash citation button on the current line; else toggle the message."
    (interactive)
    (let* ((eol (line-end-position))
	   (btn (let ((b (next-button (line-beginning-position) t)))
		  (and b (< (button-start b) eol) b))))
      (if btn
	  (push-button (button-start btn))
	(notmuch-show-toggle-message))))

  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    (kbd "RET")      #'pani/notmuch-show-ret
    (kbd "<return>") #'pani/notmuch-show-ret)

  ;; View attachments without hunting for the part button
  (defun pani/notmuch-show--attachments ()
    "Return an alist of (LABEL . POSITION) for attachments of the message at point."
    (let ((id (plist-get (notmuch-show-get-message-properties) :id))
	  (pos (point-min))
	  (seen nil)
	  (parts nil))
      (save-excursion
	(while pos
	  (when-let* ((part (get-text-property pos :notmuch-part))
		      (part-id (plist-get part :id))
		      (filename (plist-get part :filename)))
	    (goto-char pos)
	    ;; Restrict to the message point was in, and take each part once:
	    ;; a part's region can be split into several property runs.
	    (when (and (equal id (plist-get (notmuch-show-get-message-properties) :id))
		       (not (member part-id seen)))
	      (push part-id seen)
	      (push (cons (format "%s [%s]"
				  filename
				  (or (plist-get part :content-type) "?"))
			  pos)
		    parts)))
	  (setq pos (next-single-property-change pos :notmuch-part))))
      (nreverse parts)))

  (defun pani/notmuch-show-view-attachment (&optional choose-viewer)
    "View an attachment of the message at point in an external viewer."
    (interactive "P" notmuch-show-mode)
    (let ((attachments (pani/notmuch-show--attachments)))
      (unless attachments
	(user-error "No attachment in this message"))
      (let ((choice (if (= (length attachments) 1)
			(car attachments)
		      (assoc (completing-read "View attachment: " attachments nil t)
			     attachments))))
	(save-excursion
	  (goto-char (cdr choice))
	  (if choose-viewer
	      (notmuch-show-interactively-view-part)
	    (notmuch-show-view-part))))))

  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    (kbd "P") #'pani/notmuch-show-view-attachment)

  :hook ((message-send . notmuch-mua-attachment-check)
	 (message-send . pani/notmuch-mua-empty-subject-check))

  :config
  (defun pani/notmuch-poll-async ()
    "Run `mbsync -a' asynchronously, then `notmuch new' and refresh the notmuch buffer."
    (interactive)
    (if (process-live-p (get-process "mbsync"))
	(message "mbsync is already running")
      (let ((buffer (current-buffer)))
	(message "Syncing mail...")
	(make-process
	 :name "mbsync"
	 :buffer "*mbsync*"
	 :command (list (or (executable-find "mbsync") "mbsync")
			"-c" (expand-file-name "mbsync/mbsyncrc"
					       (or (getenv "XDG_CONFIG_HOME")
						   "~/.config"))
			"-a")
	 :noquery t
	 :sentinel
	 (lambda (proc event)
	   (when (memq (process-status proc) '(exit signal))
	     (if (zerop (process-exit-status proc))
		 (progn
		   (if (buffer-live-p buffer)
		       (with-current-buffer buffer
			 (notmuch-poll-and-refresh-this-buffer))
		     (notmuch-poll))
		   (message "Mail synced."))
	       (message "mbsync failed (%s) — see *mbsync*"
			(string-trim event)))))))))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'notmuch-hello-mode-map
      "gR" #'pani/notmuch-poll-async)
    (evil-collection-define-key 'normal 'notmuch-search-mode-map
      "gR" #'pani/notmuch-poll-async)
    (evil-collection-define-key 'normal 'notmuch-tree-mode-map
      "gR" #'pani/notmuch-poll-async))

  :bind
  ( :map global-map
    ("C-c m m" . notmuch)
    ("C-c m i" . pani/notmuch-jump-inbox)
    ("C-c m u" . pani/notmuch-jump-unread)
    ("C-c m t" . pani/notmuch-jump-today)
    ("C-c m p" . pani/notmuch-jump-migadu)
    ("C-c m o" . pani/notmuch-jump-orgmode)
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
    ("C-c C-o" . pani/notmuch-show-view-attachment)
    :map notmuch-hello-mode-map
    ("C-<tab>" . nil)
    ("J" . notmuch-jump-search))
  :config
  (evil-define-key 'normal notmuch-show-mode-map
    (kbd "C-j") 'notmuch-show-next-message
    (kbd "C-k") 'notmuch-show-previous-message))

;; GNUS email
(use-package gnus
  :ensure nil
  :bind (:map global-map
	      ("C-c g" . gnus))
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
	'((nnimap "migadu"
		  (nnimap-stream tls)
		  (nnimap-server-port nil)
		  (nnimap-address "imap.migadu.com")
		  (nnimap-authenticator plain)
		  (nnimap-use "ap@ashishpanigrahi.com"))
	  (nntp "gwene" (nntp-address "news.gwene.org"))
	  (nnatom "xkcd.com/atom.xml")
	  (nntp "news.yhetil.org")))

  ;; Turn on adaptive scoring
  (setq gnus-use-adaptive-scoring t)

  ;; Don't prompt when exiting with `q'
  (setopt gnus-interactive-exit nil)

  (setopt gnus-home-directory "~/.emacs.d/gnus/"
	  gnus-directory "~/.emacs.d/gnus/news/"
	  gnus-newsgroup-last-folder "~/.emacs.d/gnus/news/"
	  gnus-newsgroup-last-directory "~/.emacs.d/gnus/news/"
	  message-directory "~/.emacs.d/gnus/mail/"
	  nndraft-directory "~/.emacs.d/gnus/drafts/")

  ;; Always display certain groups
  (setopt gnus-permanently-visible-groups ":INBOX$")

  ;; Don't bother with .newsrc, use .newsrc.eld instead
  (setopt gnus-save-newsrc-file nil
	  gnus-read-newsrc-file nil)

  (setopt read-mail-command #'gnus)

  ;; Start daemon to automatically check email
  (gnus-demon-add-rescan)
  (gnus-demon-init))

;; Message composition for email
(use-package message
  :ensure nil
  :defer t
  :bind (:map message-mode-map
	      ("C-c C-e" . message-elide-region)
	      ("C-c C-z" . nil) ;; `message-kill-to-signature' is now bound to `C-c C-l'.
	      ("C-c C-l" . message-kill-to-signature))
  :hook
  (message-setup . message-sort-headers)
  :config
  (setq mail-user-agent 'message-user-agent
	message-mail-user-agent t)  ; use `mail-user-agent'
  (setq mail-header-separator "--text follows this line--")
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)

  (defun pani/message-choose-signature ()
    "Attachs signature depending on email account used."
    (let ((from (or (message-field-value "From") "")))
      (cond
       ((string-match-p "@aalto\\.fi" from)
	"Kind regards,\nAshish (Pani)\n")
       ((string-match-p "@ashishpanigrahi\\.com" from)
	"Kind regards,\nAshish\n")
       ;; Fallback (also covers the ashish@ vs public@ split if you ever need it)
       (t
	"Kind regards,\nAshish (Pani)\n"))))

  (setq message-signature #'pani/message-choose-signature
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
	;; explicitly set sendmail to use msmtp
	sendmail-program (or (executable-find "msmtp") "/usr/bin/msmtp")
        message-sendmail-envelope-from 'header))

;; Encryption settings for email
(use-package mml-sec
  :ensure nil
  :config
  (setq mm-encrypt-option nil
	mm-sign-option nil)

  (setq mml-secure-openpgp-encrypt-to-self t
	mml-secure-openpgp-sign-with-sender t)

  (defun pani/message-recipients ()
    "Return all recipients (To, Cc, Bcc) as `mail-extract-address-components' entries."
    (mapcan (lambda (header)
	      (when-let* ((value (message-fetch-field header)))
		(mail-extract-address-components value t)))
	    '("To" "Cc" "Bcc")))

  (defun pani/message-all-keys-available-p ()
    "Non-nil if the local keyring has a public key for every recipient."
    (let ((context (epg-make-context 'OpenPGP)))
      (catch 'missing
        (dolist (recipient (pani/message-recipients) t)
          (let ((email (cadr recipient)))
            (when (and email (not (epg-list-keys context email)))
              (throw 'missing nil)))))))

  (defun pani/message-sign-or-encrypt ()
    "Prompt to encrypt when possible; otherwise prompt to sign."
    (if (and (pani/message-all-keys-available-p)
	     (y-or-n-p "Encrypt? "))
	(mml-secure-message-sign-encrypt)
      (mml-secure-message-sign)))

  (add-hook 'message-send-hook #'pani/message-sign-or-encrypt))

;; Add direnv integration in emacs
;; envrc package
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  :config
  (define-advice envrc-mode (:around (orig &rest args) pani/no-remote-envrc)
    "Skip direnv in remote buffers. Run it locally as usual."
    (unless (and buffer-file-name (file-remote-p buffer-file-name))
      (apply orig args))))

;; Autocompletion via corfu
(use-package corfu
  :ensure t
  :config
  (setq corfu-cycle t) ; enable cycling for `corfu-next/previous'

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
  :hook ((python-mode . pani/eglot-ensure-local)
	 (python-ts-mode . pani/eglot-ensure-local))   ;; run eglot when opening python files
  :config
  (add-to-list 'eglot-server-programs
	       '((python-ts-mode python-mode)
		 . ("zuban" "server")))

  ;; Only have eglot work locally and not over remote machines
  (defun pani/eglot-ensure-local ()
    "Start eglot, but only for local files."
    (unless (and buffer-file-name (file-remote-p buffer-file-name))
      (eglot-ensure)))

  ;; Ensure that eglot stays out of flymake
  (setq eglot-stay-out-of '(flymake))

  ;; Disable code-action hints to reduce noise
  (setq eglot-code-action-indications nil)

  ;; (setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
  (setq eglot-ignored-server-capabilities
	'(:documentHighlightProvider
	  :inlayHintProvider
	  :colorProvider
	  :codeLensProvider
	  :semanticTokensProvider
	  :publishDiagnostics
	  :documentLinkProvider))) ;; disables highlighting words under active cursor

;; Eldoc config
(use-package eldoc
  :ensure nil  ;; built-in
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
  :hook ((inferior-python-mode . (lambda ()
				  (set-process-query-on-exit-flag
				   (get-buffer-process (current-buffer)) nil))))
  :bind
  ( :map python-mode-map
    ("C-l" . nil) ; unbind default binding for text view centering
    ("C-l" . xref-go-back)
    ("C-c C-c" . pani/python-send-or-exec)
    :map python-ts-mode-map
    ("C-l" . nil) ; unbind default binding for text view centering
    ("C-l" . xref-go-back)
    ("C-c C-c" . pani/python-send-or-exec)
    :map inferior-python-mode-map
    ("C-l" . comint-clear-buffer)  ; `C-l' clears the inferior python buffer
    ("<up>" . comint-previous-input)
    ("<down>" . comint-next-input)
  )
  :config
  (setq-local confirm-kill-processes nil)
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq python-shell-font-lock-enable nil)

  ;; Do not inform every time python tries to guess indentation
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Scroll to the bottom of the REPL on new output
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-move-point-for-output t)
  (setq compilation-scroll-output t)

  ;; Custom function to run python in an async terminal (like VS code)
  (defun pani/python-exec-file ()
    "Run the current buffer's .py file
Works for both local and TRAMP-remote buffers."
    (interactive)
    (when (buffer-file-name)
      (save-buffer)
      (let ((file-name (shell-quote-argument
			(file-relative-name (buffer-file-name)))))
	(compilation-start (format "python %s" file-name)))))

  ;; Force focus to stay put when launching inferior python
  (advice-add 'run-python :around
	      (lambda (orig-fun &rest args)
		(save-selected-window
		  (apply orig-fun args))))

  ;; Smart C-c C-c bind to run async process if inferior python process is not open
  (defun pani/python-send-or-exec ()
    "Smart `C-c C-c' for Python buffers.
If a window showing the inferior Python process is visible, send the
current buffer to it (the usual `python-shell-send-buffer' behaviour).
Otherwise, run the file asynchronously via `pani/python-exec-file'."
    (interactive)
    (let* ((proc (python-shell-get-process))
	   (buf  (and proc (process-buffer proc))))
      (if (and buf (get-buffer-window buf))
	  (python-shell-send-buffer)
	(pani/python-exec-file))))

  ;; This ensures that even if the REPL window isn't the active one, it still scrolls
  (defun pani/python-scroll-to-bottom (_string)
    (let ((window (get-buffer-window (current-buffer))))
      (when window
	(with-selected-window window
	  (goto-char (point-max))))))

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (add-hook 'comint-output-filter-functions #'pani/python-scroll-to-bottom nil t))))

;; Custom minor mode for testing python files with pytest
(use-package pani/python-pytest-binding
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun pani/python-run-pytest ()
    "Run pytest on the current buffer's .py file inside a compilation
buffer. Works for both local and TRAMP-remote buffers."
    (interactive)
    (when (buffer-file-name)
      (save-buffer)
      (let ((file-name (shell-quote-argument
			(file-relative-name (buffer-file-name)))))
	(compilation-start (format "pytest %s" file-name)))))

  (defvar pani/pytest-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'pani/python-run-pytest)
      map)
    "Keymap for `pani/pytest-mode'.")

  (define-minor-mode pani/pytest-mode
    "Minor mode to run pytest with `C-c C-c' in test files."
    :keymap pani/pytest-mode-map)

  (defun pani/python-maybe-enable-pytest ()
    "Enable `pani/pytest-mode' when visiting a `test*.py' file."
    (when (and (buffer-file-name)
	       (string-match-p "\\`test.*\\.py\\'"
			       (file-name-nondirectory (buffer-file-name))))
      (pani/pytest-mode 1)))

  (add-hook 'python-mode-hook #'pani/python-maybe-enable-pytest)
  (add-hook 'python-ts-mode-hook #'pani/python-maybe-enable-pytest))

;; code-cells for ipython like behaviour
(use-package code-cells
  :ensure t
  :init
  (add-hook 'python-ts-mode-hook 'code-cells-mode-maybe)
  (add-hook 'python-mode-hook 'code-cells-mode-maybe)
  :bind
  (:map code-cells-mode-map
    ("C-c C-c" . code-cells-eval)))

;; Elfeed for RSS
(use-package elfeed
  :vc (:url "https://github.com/emacs-elfeed/elfeed"
       :rev :newest)
  :defer t
  :hook ((elfeed-search-mode . variable-pitch-mode)
	 (elfeed-show-mode . variable-pitch-mode))
  :bind (("C-c e" . elfeed)
	 :map elfeed-search-mode-map
	 ("C-c C-c" . elfeed-update)
	 ("C-c C-l" . pani/elfeed-open-link)
	 ("C-c C-o" . pani/elfeed-arxiv-open-pdf)
	 :map elfeed-tree-mode-map
	 ("C-c C-c" . elfeed-update)
	 :map elfeed-show-mode-map
	 ("C-c C-o" . pani/elfeed-arxiv-open-pdf)
	 ("C-c C-l" . pani/elfeed-open-link))
  :config
  (defun pani/elfeed-fix-width-after-render (&rest _)
    "Re-flow arXiv abstracts to `fill-column' after Elfeed renders them.
Only arXiv entries need this: their abstracts arrive as plain text with
hard mid-sentence newlines.  Other feeds deliver real HTML that shr
already lays out correctly, so re-filling them destroys lists and other
block structure."
    (when (and (eq major-mode 'elfeed-show-mode)
	       (elfeed-tagged-p 'arxiv elfeed-show-entry))
      (let ((inhibit-read-only t)
	    (fill-column 90))
	(save-excursion
	  (goto-char (point-min))
	  (when (search-forward "\n\n" nil t)
	    (fill-region (point) (point-max)))))))

  (advice-add 'elfeed-show-refresh :after #'pani/elfeed-fix-width-after-render)

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (cond
     ((null authors-list) "")
     ((> (length authors-list) 1)
      (format "%s et al." (or (plist-get (nth 0 authors-list) :name) "")))
     (t (or (plist-get (nth 0 authors-list) :name) ""))))

  ;; Custom color for the authors column.
  (defface elfeed-search-author-face
    '((t :foreground "DarkOliveGreen3"))
    "Face for displaying authors in the elfeed search buffer.")

  (defun pani/elfeed-search-print-entry (entry)
    "Print ENTRY for the `elfeed-search' buffer.
arXiv entries show: title | authors | right-aligned score.
All other entries show: title | feed name (no score)."
    (let* ((title     (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
	   (faces     (elfeed-search--faces (elfeed-entry-tags entry)))
	   (win-width (window-width)))
      (if (elfeed-tagged-p 'arxiv entry)
	  ;; --- arXiv: date | title | authors | score ---
	  (let* ((authors       (concatenate-authors (elfeed-meta entry :authors)))
		 (score         (number-to-string
				 (or (ignore-errors
				       (elfeed-score-scoring-get-score-from-entry entry))
				     0)))
		 (score-width   4) ; fixed slot; covers scores up to 9999
		 (authors-width (elfeed-clamp 10 (/ win-width 2) 25))
		 (title-width   (max 10 (- win-width authors-width 1
					   score-width 1))))
	    (insert (propertize (elfeed-format-column title title-width :left)
				'face faces 'kbd-help title)
		    " ")
	    (insert (propertize (elfeed-format-column authors authors-width :left)
				'face 'elfeed-search-author-face 'kbd-help authors))
	    (insert (propertize " " 'display
				`(space :align-to (- right ,score-width)))
		    (elfeed-format-column score score-width :right)))
	;; --- everything else: title | feed name ---
	(let* ((feed-name (or (when-let* ((feed (elfeed-entry-feed entry)))
				(elfeed-meta--title feed))
			      ""))
	       (feed-width  (elfeed-clamp 10 (/ win-width 4) 40))
	       (title-width (max 10 (- win-width feed-width 1 1))))
	  (insert (propertize (elfeed-format-column title title-width :left)
			      'face faces 'kbd-help title)
		  " ")
	  (insert (propertize (elfeed-format-column feed-name feed-width :left)
			      'face 'elfeed-search-feed-face 'kbd-help feed-name))))))

  (defun pani/elfeed-unify-fonts ()
    "Remap faces to variable-pitch and scale buffer text by exactly 1.15x."
    ;; 1. Force fixed-width text (arXiv abstracts) to use the variable-pitch face.
    (face-remap-add-relative 'fixed-pitch 'variable-pitch)
    (face-remap-add-relative 'shr-code 'variable-pitch)
    (face-remap-add-relative 'shr-abbreviation 'variable-pitch)
    ;; 2. Scale the buffer content by exactly 1.15 (1.05 x 1.15 = 1.20).
    ;;    Remapping `default' locally scales the buffer text but leaves the
    ;;    modeline (which uses the unscaled global faces) alone.
    (face-remap-add-relative 'default :height 1.15)
    ;; 3. Fix olivetti layout for this bigger font.
    (setq-local olivetti-body-width 0.9)
    (setq-local olivetti-minimum-body-width 130)
    (olivetti-mode 1))
  (add-hook 'elfeed-show-mode-hook #'pani/elfeed-unify-fonts)

  ;; Remove the horizontal-line month separator.
  ;; (see https://github.com/emacs-elfeed/elfeed/issues/602#issuecomment-4472279716)
  (remove-hook 'elfeed-search-update-hook #'elfeed-search-add-separators)

  ;; Open the arXiv PDF for the current entry in a browser.
  (defun pani/elfeed-arxiv-open-pdf (entry)
    "Open the arXiv PDF for the current Elfeed ENTRY in a browser.
Works in both `elfeed-search-mode' and `elfeed-show-mode'."
    (interactive
     (list (cond
	    ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected :single))
	    ((derived-mode-p 'elfeed-show-mode)   elfeed-show-entry))))
    (when entry
      (let ((link (elfeed-entry-link entry)))
	(if (string-match "arxiv\\.org/abs/\\([0-9.]+\\)" link)
	    (browse-url (format "https://arxiv.org/pdf/%s.pdf" (match-string 1 link)))
	  (message "Not an arXiv link: %s" link)))))

  ;; Toggle between arxiv and non-arxiv entries
  (defun pani/elfeed-toggle-arxiv-filter ()
    "Toggle the search filter between arXiv-only and everything but arxiv."
    (interactive nil elfeed-search-mode)
    (setq elfeed-search-filter
	  (if (string-match-p "[+]arxiv" elfeed-search-filter)
	      "@6-months-ago -arxiv"
	    "@6-months-ago +arxiv"))
    (elfeed-search-update :force))

  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "a") #'pani/elfeed-toggle-arxiv-filter)

  (defun pani/elfeed-open-link (entry)
    "Open ENTRY's main article link in the browser.
Works in both `elfeed-search-mode' and `elfeed-show-mode'."
    (interactive
     (list (cond
	    ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected :single))
	    ((derived-mode-p 'elfeed-show-mode)   elfeed-show-entry))))
    (unless entry
      (user-error "No Elfeed entry at point"))
    (let ((link (elfeed-entry-link entry)))
      (if link
	  (browse-url link)
	(message "Entry has no link"))))

  (setq elfeed-search-print-entry-function #'pani/elfeed-search-print-entry)
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq-default elfeed-search-filter "@6-months-ago +arxiv")
  (setq elfeed-db-directory "~/.emacs.d/elfeed/")
  (setq elfeed-enclosure-default-dir "~/downloads/")
  (setq elfeed-feeds
	'(("http://export.arxiv.org/api/query?search_query=cat:quant-ph+cond-mat.mes-hall&start=0&max_results=500&sortBy=submittedDate&sortOrder=descending"
	   arxiv)
	  ("http://emacsredux.com/atom.xml" emacs)
	  ("http://lambdaland.org/atom.xml" :title "lambdaland.org" blog emacs)
	  ("https://bwestbro.com/rss" blog emacs)
	  ("https://magnus.therning.org/feed.xml" emacs)
	  ("http://www.emacs.dyerdwelling.family/index.xml" emacs)
	  ("http://sachachua.com/blog/category/emacs/feed/index.xml" :title "Sacha Chua" emacs)
	  ("http://karthinks.com/index.xml" :title "Karthinks" emacs)
	  ("https://feyor.sh/atom.xml" emacs blog)
	  ("https://thanosapollo.org/tags/emacs/index.xml" emacs)
	  ("http://ogbe.net/blog-feed.xml" emacs blog)
	  ("http://protesilaos.com/codelog.xml" :title "Prot: Codelog" emacs)
	  ("http://protesilaos.com/commentary.xml" :title "Prot: Commentary" blog)
	  ("http://protesilaos.com/news.xml" :title "Prot: News" blog)
	  ("http://protesilaos.com/poems.xml" :title "Prot: Poems" blog)
	  ("http://themkat.net/feed.xml" emacs)
	  ("https://emacs.stackexchange.com/feeds" :title "Emacs Stackexchange" emacs forum)
	  ("http://www.reddit.com/r/orgmode.rss" :title "r/orgmode" emacs orgmode forum)
	  ("http://www.reddit.com/r/emacs.rss" :title "r/emacs" emacs orgmode forum)
	  ("https://coredumped.dev/index.xml" emacs)
	  ("http://irreal.org/blog/?tag=emacs&feed=rss2" emacs)
	  ("https://ray-on-emacs.blogspot.com/feeds/posts/default" :title "Ray on Emacs" emacs)
	  ("http://marci.gunyho.com/rss" blog)
	  ("http://11de784a.github.io/feed.xml" blog)
	  ("http://terrytao.wordpress.com/feed/" :title "Terry Tao" blog)
	  ("http://m-malinowski.github.io/feed.xml" blog)
	  ("https://ashishpanigrahi.com/atom.xml" blog)
	  ("https://theprivacydad.com/feed/" blog)
	  ("http://timharek.no/rss.xml" blog)
	  ("http://matt.might.net/articles/feed.rss" blog)
	  ("http://protesilaos.com/commentary.xml" blog)
	  ("https://karpathy.bearblog.dev/feed/" :title "Karpathy AI" blog)
	  ("http://gregorygundersen.com/feed.xml" blog)
	  ("http://rosenzweig.io/feed.xml" blog)
	  ("http://adol.pw/index.xml" blog)
	  ("https://stackoverflow.com/feeds/question/77369042" forum)
	  ("https://scottaaronson.blog/?feed=rss2" :title "Scott Aaronson" blog research)
	  ("https://nicholas.carlini.com/writing/feed.xml" blog research)
	  ("http://tony-zorman.com/atom.xml" blog)
	  ("http://andreyor.st/feed.xml" blog)
	  ("http://www.paritybit.ca/feed.xml" blog)
	  ("http://kishvanchee.com/index.xml" blog)
	  ("https://www.cosroe.com/feed.atom" blog)
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
	  ("http://xkcd.com/atom.xml" :title "xkcd" comics)
	  ("http://www.smbc-comics.com/comic/rss" :title "SMBC" comics)
	  ("https://warandpeas.com/feed/" :title "War and Peas" comics)
	  ("http://www.commitstrip.com/en/feed/" comics))))

(use-package elfeed-score
  :ensure t
  :after elfeed
  :config
  (setq elfeed-score-rule-stats-file "~/.emacs.d/elfeed.stats")
  (setq elfeed-score-serde-score-file "~/.emacs.d/elfeed.score")
  (elfeed-score-enable))

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
  :ensure nil
  :config
  (global-auto-revert-mode 1))

;; Info-mode config
(use-package info
  :ensure nil ; built-in
  :hook (Info-mode . variable-pitch-mode)
  :config
  (evil-define-key 'normal Info-mode-map
    (kbd "SPC") nil
    (kbd "u") #'Info-up
    (kbd "C-j") #'Info-next
    (kbd "C-k") #'Info-prev)

  ;; Custom function to resize fonts in Info manuals
  (defun pani/info-font-size ()
    "Remap the variable-pitch font face to 1.15 height specifically for
Info manuals."
    (face-remap-add-relative 'default :height 1.15))

  (add-hook 'Info-mode-hook #'pani/info-font-size))

(use-package tramp
  :ensure nil ; built-in
  :config
  (setq tramp-verbose 1)
  (setq tramp-terminal-type "dumb")

  ;; Set tramp debug buffer to text-mode instead of default outline-mode
  (defun my-tramp-debug-to-text-mode ()
    "Force Tramp debug buffers into text-mode."
    (when (and (string-match-p "\\*debug tramp/" (buffer-name))
	       (eq major-mode 'outline-mode))
      (text-mode)))

  (add-hook 'after-change-major-mode-hook #'my-tramp-debug-to-text-mode)

  (add-to-list 'tramp-remote-path
	       "/c/Users/PXI05/qcodes-experiments/panigrahi/xilinx-venv/Scripts"))

;; Treesitter config
(use-package treesit
  :ensure nil ; built-in
  :config
  ;; Remap major modes to treesitter
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode)
	  (markdown-mode . markdown-ts-mode)
	  (conf-toml-mode . toml-ts-mode)))
  ;; Set sources for treesitter grammar
  (setq treesit-language-source-alist
	`((python "https://github.com/tree-sitter/tree-sitter-python"
		  ,(when (< (treesit-library-abi-version) 15) "v0.23.6")) ;; use v0.23.6 if treesitter ABI version is < 15.
	  (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
		    ,(if (< (treesit-library-abi-version) 15) "v0.4.1" "split_parser")
		    "tree-sitter-markdown/src")
	  (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
			   ,(if (< (treesit-library-abi-version) 15) "v0.4.1" "split_parser")
			   "tree-sitter-markdown-inline/src")
	  (bash "https://github.com/tree-sitter/tree-sitter-bash"
		,(when (< (treesit-library-abi-version) 15) "v0.23.3"))
	  (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (typst "https://github.com/uben0/tree-sitter-typst")))
  ;; Tree-sitter font lock (4 is highest)
  (setq treesit-font-lock-level 4)

  ;; Auto-install/repair any configured grammar files that won't load properly
  (dolist (lang (mapcar #'cat treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

;; Typst support
(use-package typst-ts-mode
  :ensure t
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
	erc-log-insert-log-on-open 'erc-log-new-target-buffer-p
	erc-fill-function 'erc-fill-static
	erc-fill-static-center 20))

;; ;; Smooth scrolling
;; (use-package pixel-scroll
;;   :ensure nil
;;   :bind (:map global-map
;; 	      ([remap scroll-up-command] . pixel-scroll-interpolate-down)
;; 	      ([remap scroll-down-command] . pixel-scroll-interpolate-up))
;;   :custom
;;   (pixel-scroll-precision-interpolate page t)
;;   (pixel-scroll-precision-mode t)
;;   :config
;;   (pixel-scroll-precision-mode 1))

(use-package ultra-scroll
  :ensure t
  :init
  (setq scroll-conservatively 101)
  (setq scroll-margin 0)
  (setq pixel-scroll-precision-interpolate-page t)
  :config
  (setq make-cursor-line-fully-visible t)
  (ultra-scroll-mode 1))

;; ediff configuration
(use-package ediff
  :ensure nil ; built-in
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-show-clashes-only t)

  (setq ediff-split-window-function 'split-window-sensibly)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; isearch config
(use-package isearch
  :ensure nil ; built-in
  :bind (:map global-map
	      ("C-s" . isearch-forward))
  :config
  ;; Search for words not necessarily one after the other
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-count t)
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-cleanup t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq isearch-wrap-pause 'no-ding)) ; Jump to top/bottom without asking

;; ibuffer settings
(use-package ibuffer
  :ensure nil ; built-in
  :hook
  ((Buffer-menu-mode . hl-line-mode))
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-human-readable-size t)
  (setq ibuffer-use-other-window nil))

;; Highlight TODO keywords in code
(use-package hl-todo
  :ensure t
  :hook ((python-mode . hl-todo-mode)
	 (python-ts-mode . hl-todo-mode)
	 (typst-ts-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
	'(("TODO" . "#F59082"))))

(use-package breadcrumb
  :ensure t
  :defer 5
  :hook ((python-mode . breadcrumb-local-mode)
	 (python-ts-mode . breadcrumb-local-mode)))

;; Highlight parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Spellchecker
;; Needs libenchant, pkgconf and dictionaries (aspell and aspell-en)
;; installed on system
(use-package jinx
  :ensure t
  :defer t
  :config
  (setq jinx-languages "en"))

;; Templates for faster text
(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
	 ("M-*" . tempel-insert)
	 :map tempel-map
	 ("TAB" . tempel-next)
	 ("<tab>" . tempel-next)
	 ("S-TAB" . tempel-previous)
	 ("<backtab>" . tempel-previous)
	 ("C-g" . tempel-done))
  :init
  ;; Setup completion at point
  (defun pani/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
		(cons #'tempel-expand completion-at-point-functions)))

  (dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
    (add-hook hook #'pani/tempel-setup-capf))

  (defun pani/org-tempel-tab ()
    "Expand the template at point. Non-nil if one was expanded."
    (condition-case nil
	(tempel-complete t)
      (user-error nil)))

  (with-eval-after-load 'org
    (add-hook 'org-cycle-tab-first-hook #'pani/org-tempel-tab))

  :config
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

;; Music management with EMMS
(use-package emms
  :ensure t
  :commands (emms emms-play-directory-tree emms-browser)
  :config
  (setq emms-player-list '(emms-player-mpv)
	emms-info-functions '(emms-info-native)
	emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-source-file-default-directory "/l/music/")
  (emms-all))
