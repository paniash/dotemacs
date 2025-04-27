(setq inhibit-startup-message t)

;; Hack to increase startup speed
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

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

(setq inhibit-compacting-font-caches t)

;; Resizing emacs frame can be terribly expensive part of changing
;; font. By inhibiting this, we easily halve the startup times with
;; fonts that are larger than the system default
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))
