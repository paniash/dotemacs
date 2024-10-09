(setq inhibit-startup-message t)

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

;; No more saving customization information
(setq custom-file null-device)

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
