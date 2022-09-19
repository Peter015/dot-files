;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; installing packages

;; evil mode
(use-package evil
    :config
    (evil-mode 1)
    (evil-set-initial-state 'NeoTree 'emacs))

;; treesitter packages
(use-package tree-sitter)
(use-package tree-sitter-langs)

;; lsp
(use-package eglot)

;; merlin for ocaml
(use-package merlin)

;; parens
(use-package paredit)

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode) 

(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t))

    
;; neotree            
(use-package neotree)
(global-set-key [f12] 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
(setq neo-smart-open t)
(add-hook 'after-init-hook #'neotree-toggle)

;; icons
(use-package all-the-icons
  :if (display-graphic-p))

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    
      
                          
;; settings
(setq inhibit-startup-message t
    visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode)

;; disabling/enabling
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; cua mode for more standard keyboard commands
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; set the theme
(use-package sublime-themes)
(use-package tron-legacy-theme
  :config
  (load-theme 'tron-legacy t))

;; neotree theme
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


;; utf-8 encoding
(set-language-environment "UTF-8")


;; treesitter
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; merlin support
(push "/home/peterz/.config/emacs/elpa" load-path)
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook #'merlin-mode)
(add-hook 'caml-mode-hook #'merlin-mode)


;; sets up slime, common lisp IDE based on emacs
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy slime-quicklisp slime-asdf))
;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)












(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(parinfer-rust-mode paredit use-package undo-tree tree-sitter-langs smartparens rainbow-mode rainbow-delimiters merlin major-mode-hydra ivy-prescient ivy-hydra feebleline expand-region evil emojify eglot counsel command-log-mode avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

