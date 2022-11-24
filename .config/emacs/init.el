;; Packages installed via Nix when possible, otherwise use-package is used

;; speed up startup -- undone in a hook at end of file
(setq gc-cons-threshold 100000000)

(defvar startup/file-name-handler-alist file-name-handler-alist) ; used later in the hook to reset file-name-handler-alist
(setq file-name-handler-alist nil)

;; ivy
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x) 
(global-set-key "\C-s" 'swiper)
(ivy-rich-mode 1)

;; which key
(which-key-mode)
(which-key-setup-minibuffer)


;; theme
(defun onoff (theme1 theme2)
    (disable-theme theme1)
    (enable-theme theme2))

(defun set-theme-time ()
  (let ((light 'modus-operandi)
        (dark 'modus-vivendi))
	(load-theme light t t)
	(load-theme dark t t)
	(run-at-time "8:00" nil #'onoff dark light)
	(run-at-time "17:00" nil #'onoff light dark)
	(message "Theme Loaded")))
(set-theme-time)

(menu-bar-mode	 -1) ; turn off menu bar
(tool-bar-mode   -1) ; turn off tool bar
(scroll-bar-mode -1) ; turn off scroll bar

;; set font
(set-face-attribute 'default nil :family "IBM Plex Mono")

;; other settings
(setq visible-bell t
	  backup-directory-alist `(("." . "~/Backups/emacs/"))
	  backup-by-copying t)
(setq-default cursor-type 'bar
	      indent-tabs-mode t
	      tab-width 4)
(prefer-coding-system 'utf-8)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; disable for some modes -- doesn't work to add prog mode hook to display line numbers
(dolist (mode '(org-mode-hook
				 markdown-mode-hook
				 eshell-mode-hook
				 term-mode-hook
				 neotree-mode-hook
				 slime-mode-hook
				 slime-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; undo-tree
(global-undo-tree-mode)

;; minimap/neotree
(setq minimap-window-location 'right)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

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

;; open files as root
(global-set-key (kbd "C-c C-r") 'sudo-edit)

;; define when to open certain mode
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)
			 '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))

;; custom fuctions
(defun comment-or-uncomment-region-or-line ()
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(defun my-ide-like ()
  (neotree-show)
  (minimap-mode t))

(defun my-doc ()
  (neotree-hide)
  (minimap-mode 0)
  (darkroom-mode))

(defun my-open-term ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (ansi-term "/home/peterz/.nix-profile/bin/fish"))

;; custom bindings
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c t") 'my-open-term)

;; hooks
(add-hook 'org-mode-hook      #'my-doc)
(add-hook 'markdown-mode-hook #'my-doc)

(add-hook 'prog-mode-hook #'my-ide-like)

(add-hook 'prog-mode-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(add-hook 'prog-mode-hook  'highlight-indent-guides-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'lisp-mode-hook '(lambda ()
                             (unless (get-process "SLIME Lisp")
                               (let ((oldbuff (current-buffer)))
                                 (slime)
                                 (switch-buffer oldbuff)))))
		  
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

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist startup/file-name-handler-alist)))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))





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
