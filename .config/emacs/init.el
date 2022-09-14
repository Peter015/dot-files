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
(use-package evil
    :config
    (evil-mode 1)
    (evil-set-initial-state 'NeoTree 'emacs))

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


;; ivy configuration


;; sets up slime, common lisp IDE based on emacs
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
