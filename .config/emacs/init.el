;; Packages installed via Nix when possible, otherwise use-package is used
;; Speed up startup -- undone in a Hook at end of file
(setq gc-cons-threshold 1000000000)
(defvar startup/file-name-handler-alist file-name-handler-alist) ; used later in the hook to reset file-name-handler-alist
(setq file-name-handler-alist nil)

;; startup message
(message (format "Emacs ready in %.2f seconds with %d garbage collections."
                 (float-time (time-subtract after-init-time before-init-time)) gcs-done))

;; Ivy
(ivy-mode 1)
(ivy-rich-mode 1)

;; which key
(which-key-mode)
(which-key-setup-minibuffer)

(menu-bar-mode	 -1) ; turn off menu bar
(tool-bar-mode   -1) ; turn off tool bar
(scroll-bar-mode -1) ; turn off scroll bar

;; set font
(set-face-attribute 'default nil :family "IBM Plex Mono")

;; god mode for global editing
(setq god-mode-enable-function-key-translation nil)
(god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'post-command-hook (lambda()
															 (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar))))

;; other settings
(setq visible-bell t
	  backup-directory-alist `(("." . "~/Backups/emacs/"))
	  backup-by-copying t
		auto-compile-display-buffer nil
		auto-compile-mode-line-counter t
	  inferior-lisp-program "/run/current-system/sw/bin/sbcl"
	  ruby-indent-tabs-mode t
		inhibit-startup-screen t)
(setq-default cursor-type 'box
	      indent-tabs-mode t
	      tab-width 2
				auto-fill-function nil
				truncate-lines t)
(prefer-coding-system 'utf-8)

;; using tabs to indent and not spaces
(defvaralias 'c-basic-offset    'tab-width)
(defvaralias 'ruby-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

;; global modes
(column-number-mode)
(global-display-line-numbers-mode t)
(global-tree-sitter-mode)
(global-flycheck-mode)
(global-tab-line-mode t)
(global-visual-line-mode 0)


;; minimap
(setq minimap-window-location 'right)

;; enable cua mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; projectile mode
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Projects/"))
(setq projectile-switch-project-action #'projectile-dired)
(counsel-projectile-mode)

;; tide for typescript
(defun setup-tide-mode ()
	(interactive)
	(tide-setup)
	(flycheck-mode +1)
	(setq flycheck-check-syntax-automatically '(save mode-enabled))
	(eldoc-mode +1)
	(tide-hl-identifier-mode +1)
	(company-mode +1))

;; smartparens
(require 'smartparens-config)
(sp-use-smartparens-bindings)

;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; define when to open certain mode
(append auto-mode-alist '(("\\.nix\\'" . nix-mode)
                          ("\\.php\\'" . php-mode)))

;; modeline
(setq sml/no-confirm-load-theme t)

;; tabline
(setq tab-line-new-button-show nil
			tab-line-close-button-show nil)

;; custom fuctions
(defun onoff (theme1 theme2)
    (disable-theme theme1)
    (enable-theme theme2))

(defun set-theme-time ()
 (let ((light 'modus-operandi)
       (dark 'modus-vivendi))
	(load-theme light t t)
	(load-theme dark t t)
	(run-at-time "0:00" nil #'enable-theme dark)
	(run-at-time "8:00" nil #'onoff dark light)
	(run-at-time "17:00" nil #'onoff light dark)))

(defun comment-or-uncomment-region-or-line ()
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
					(setq beg (line-beginning-position) end (line-end-position)))
				(comment-or-uncomment-region beg end)
				(next-line)))

(defun my-open-term ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (ansi-term "/home/peterz/.nix-profile/bin/fish"))

;; custom global bindings
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line) ; comment/uncomment
(global-set-key (kbd "C-c t") 'my-open-term) ; run a terminal in a horizontal window
(eval-after-load 'sly-mrepl
   '(define-key sly-mrepl-mode-map (kbd "C-l") ; C-l to clear sly REPL
      'sly-mrepl-clear-repl))
(global-set-key (kbd "M-x") 'counsel-M-x) ; counsel mx vs default mx
(global-set-key "\C-s" 'swiper) ; swiper to search files

;; hook to restore things to normal state
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  file-name-handler-alist startup/file-name-handler-alist)))

;; hooks
(add-hook 'prog-mode-hook 'minimap-mode t)
(add-hook 'prog-mode-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))
(add-hook 'prog-mode-hook  'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'smartparens-mode)

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-mode-hook       'racket-repl)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook (lambda ()(local-set-key (kbd "C-l") 'racket-repl-clear-leaving-last-prompt)))

(dolist (mode '(eshell-mode-hook
								term-mode-hook
								sly-mrepl-mode-hook
								racket-repl-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
								markdown-mode-hook
								dashboard-mode-hook))
  (add-hook mode (lambda()
				   (display-line-numbers-mode 0)
				   (minimap-mode 0)
				   (darkroom-mode))))

(dolist (mode '(sly-mrepl-mode-hook
                racket-repl-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook
								racket-mode-hook))
	(add-hook mode #'smartparens-strict-mode))

(dolist (mode '(typescript-mode-hook
								nix-mode-hook
								c-mode-hook
								c++-mode-hook
								python-mode-hook
								racket-mode-hook))
	(add-hook mode #'eglot-ensure))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'before-save-hook      'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'after-init-hook #'set-theme-time)
(add-hook 'after-init-hook #'sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
