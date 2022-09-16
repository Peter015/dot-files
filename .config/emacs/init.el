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

;; ivy
(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
        :map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)
        ("C-l" . ivy-alt-done)
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        :map ivy-switch-buffer-map
        ("C-k . ivy-previous-line")
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map
        ("C-k" . ivy-previous-line)
        ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))
        

;; settings
(setq inhibit-startup-message t
    visible-bell t
    ivy-initial-inputs-alist nil
    )

(defalias 'yes-or-no-p 'y-or-n-p)


;; disabling/enabling
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; set the theme
(load-theme 'wombat t)


;; eglot autoconnet
(add-hook 'foo-mode-hook 'eglot-ensure)


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
