;; Global GC bump for startup
(setq gc-cons-threshold 100000000)

;; Disable file name handler (speed up loading .el files)
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 2000000
		  file-name-handler-alist file-name-handler-alist-old)))

;; Prevent package.el from loading too early
(setq package-enable-at-startup nil)

;; Silence native-comp messages at startup (does pop up in the minibuffer)
(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-message t)

;; Hack to increase startup speed
(setq vc-handled-backends '(Git))

(setq-default default-frame-alist '(
				    ;; Setting the face in here prevents flashes of
				    ;; color as the theme gets activated
				    (background-color . "#130911")
				    (ns-appearance . dark)
				    (ns-transparent-titlebar . t)
				    (tool-bar-lines . 0)
				    (menu-bar-lines . 0)
				    (vertical-scroll-bars . nil)
				    (horizontal-scroll-bars . nil)))

;; No more saving customization information to init.el
;; Setting the variable to `null-device' would yield a maximum buffer size issue
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :no-error-if-file-is-missing)

;; Fixes UI font issue when emacs is run in daemon mode
(defun efs/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Hack" :height 115)
  (set-face-attribute 'fixed-pitch nil :font "Hack" :height 115)
  (set-face-attribute 'variable-pitch nil :font "Iosevka" :height 125))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (efs/set-font-faces))))
  (efs/set-font-faces))

;; Set name for main frame
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

;; Better window management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

;; Resizing emacs frame can be terribly expensive part of changing
;; font. By inhibiting this, we easily halve the startup times with
;; fonts that are larger than the system default
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))
