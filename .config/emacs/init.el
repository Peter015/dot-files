;; Packages installed via Nix when possible, otherwise use-package is used
;; byte compile
(setq load-prefer-newer t)
(auto-compile-on-save-mode)

;; Speed up startup -- undone in a Hook at end of file
(setq gc-cons-threshold 100000000)
(defvar startup/file-name-handler-alist file-name-handler-alist) ; used later in the hook to reset file-name-handler-alist
(setq file-name-handler-alist nil)

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

;; other settings
(setq visible-bell t
	  backup-directory-alist `(("." . "~/Backups/emacs/"))
	  backup-by-copying t
		auto-compile-display-buffer nil
		auto-compile-mode-line-counter t
	  inferior-lisp-program "/run/current-system/sw/bin/sbcl"
	  ruby-indent-tabs-mode t)
(setq-default cursor-type 'bar
	      indent-tabs-mode t
	      tab-width 2)
(prefer-coding-system 'utf-8)

;; using tabs to indent and not spaces
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'ruby-indent-level 'tab-width)
(defvaralias 'sgml-basic-offset 'tab-width)

;; global modes
(column-number-mode)
(global-display-line-numbers-mode t)

;; global modes
(global-undo-tree-mode)
(global-tree-sitter-mode)
(global-flycheck-mode)

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

;; dashboard
(setq dashboard-items '((recents . 5)
						(projects . 5)))
(setq dashboard-startup-banner 'official)
(dashboard-setup-startup-hook)
(setq dashboard-set-heading-icons t
	  dashboard-set-file-icons t
	  dashboard-banner-logo-title
        (format "Emacs ready with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)
		dashboard-set-footer nil)

;; tide for typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

;; smartparens
(require 'smartparens-config)
(sp-use-smartparens-bindings)

;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; define when to open certain mode
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)
			 '("\\.php\\'" . php-mode))

;; modeline
(setq sml/no-confirm-load-theme t)

;; custom fuctions
(defun onoff (theme1 theme2)
    (disable-theme theme1)
    (enable-theme theme2))

(defun set-theme-time ()
  (let ((light 'modus-operandi)
        (dark 'modus-vivendi))
	(load-theme light t t)
	(load-theme dark t t)
	(run-at-time "0:00" nil #'onoff light dark)
	(run-at-time "8:00" nil #'onoff dark light)
	(run-at-time "17:00" nil #'enable-theme dark)))

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
(global-set-key (kbd "M-x") 'counsel-M-x) ; counsel mx vs vanilla mx
(global-set-key "\C-s" 'swiper) ; swiper to search files
;; (global-set-key (kbd "C-c M-c") 'mc/edit-lines) ; multiple cursor support
;; (define-key mc/keymap (kbd "<return>") nil) ; make space behave normally while using multiple cursors
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this) ; mark next line that matches the current line
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this) ; mark previous line that matches the current line
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this) ; mark all lines that match the current line
;; (global-set-key (kdb "C-c M-c") 'set-rectangular-region-anchor) ; set rectangle anchor to move the cursors up and down


;; hooks
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
				 markdown-mode-hook))
  (add-hook mode (lambda()
				   (display-line-numbers-mode 0)
				   (minimap-mode 0)
				   (darkroom-mode))))

(dolist (mode '(sly-mrepl-mode-hook
								racket-repl-mode-hook
								racket-mode-hook
								lisp-mode-hook
								emacs-lisp-mode-hook))
	(add-hook mode 'smartparens-strict-mode))

(add-hook 'prog-mode-hook 'minimap-mode t)
(add-hook 'prog-mode-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))
(add-hook 'prog-mode-hook  'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook  'smartparens-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'before-save-hook      'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook	      'eglot-ensure)
(add-hook 'c-mode-hook          'eglot-ensure)
(add-hook 'c++-mode-hook        'eglot-ensure)
(add-hook 'python-mode-hook     'eglot-ensure)
(add-hook 'racket-mode-hook     'eglot-ensure)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist startup/file-name-handler-alist)))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

(add-hook 'after-init-hook 'dashboard-refresh-buffer)
(add-hook 'after-init-hook #'set-theme-time)
(add-hook 'after-init-hook #'sml/setup)

(add-hook 'dashboard-mode-hook 'my/dashboard-banner)
