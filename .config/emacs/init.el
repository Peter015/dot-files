;;;  Emacs config file. Packages installed via Nix when possible, otherwise use-package is used

;; speed up startup
(setq gc-cons-threshold 100000000)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist)) ; reset for after init

(defun startup/reset-gc ()
  (setq gc-cons-threshold 800000)) ; reset for after init

;; ivy
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key "\C-s" 'swiper)
(ivy-rich-mode 1)

;; which key
(which-key-mode)
(which-key-setup-minibuffer)

;; theme
(setq circadian-themes '(("8:00" . modus-operandi)
						 ("16:30" . modus-vivendi)))
(circadian-setup)

(menu-bar-mode					 -1) ; turn off menu bar
(tool-bar-mode   				 -1) ; turn off tool bar
(scroll-bar-mode 				 -1) ; turn off scroll bar

;; set font
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans") ; tbh idk what this line does
(set-face-attribute 'default        nil :family "IBM Plex Mono")

;; other settings
(setq visible-bell t
	  highlight-indent-guides-method 'bitmap
	  backup-directory-alist `(("." . "~/Backups/emacs/"))
	  backup-by-copying t)
	
(setq-default cursor-type 'bar
	      indent-tabs-mode t
	      tab-width 4)

(prefer-coding-system 'utf-8)

;; enable cua mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; projectile mode
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Projects/" "~/common-lisp"))
(setq projectile-switch-project-action #'projectile-dired)
(counsel-projectile-mode)

;; dashboard
; (defun my-dashboard-banner ()
;  (format "Emacs ready with %d garbage collections" gcs-done)
(setq dashboard-items '((recents . 5)
						(projects . 5)))

						
(setq dashboard-startup-banner 'official)
(dashboard-setup-startup-hook)

;; slime
(setq inferior-lisp-program "sbcl")

;; enable tree sitter
(global-tree-sitter-mode)

;; enable flycheck syntax checking
(global-flycheck-mode)

;; tide for typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1))

;; python
(setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; define when to open certain modes
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)
			 '("\\.php\\'" . php-mode))

;; open files as root
(global-set-key (kbd "C-c C-r") 'sudo-edit)

;; hooks
(add-hook 'prog-mode-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode 1)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'slime-mode-hook      (lambda() (local-set-key (kbd "C-l") 'slime-repl-clear-buffer)))
(add-hook 'slime-repl-mode-hook (lambda() (local-set-key (kbd "C-l") 'slime-repl-clear-buffer)))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook		#'enable-paredit-mode)
(add-hook 'scheme-mode-hook		#'enable-paredit-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'before-save-hook      'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook	    'eglot-ensure)
(add-hook 'c-mode-hook          'eglot-ensure)
(add-hook 'c++-mode-hook        'eglot-ensure)
(add-hook 'python-mode-hook     'eglot-ensure)

(add-hook 'after-init-hook     'dashboard-refresh-buffer)
(add-hook 'dashboard-mode-hook 'my-dashboard-banner)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(circadian which-key use-package tree-sitter-langs tide slime rainbow-delimiters php-mode paredit page-break-lines nix-mode monokai-pro-theme markdown-mode magit ivy-rich highlight-indent-guides focus eglot dashboard counsel-projectile company all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
