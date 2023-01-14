;;; init.el -- NickMass' emacs config
;;; Commentary:
;;; Rust and web dev ahoy.

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; Appearence
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(setq inhibit-startup-screen 1)

(global-hl-line-mode 1)

(set-face-attribute 'default nil
		:family "Pragmata Pro"
		:height 160
		:weight 'bold)


;; Behavior
(server-start)
(setq create-lockfiles nil)

;; Store backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(show-paren-mode 1)
(global-so-long-mode 1)

(setq split-height-threshold 160)
(setq-default show-trailing-whitespace 1)
(setq-default display-line-numbers 'relative)
(setq auto-hscroll-mode 'current-line)
(setq-default truncate-lines 1)
(setq-default truncate-partial-width-windows 1)

;; lsp-mode suggested performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Shift+Arrows to navigate windows
(windmove-default-keybindings)

;; Persist hardlinks to files
(setq backup-by-copying 1)

;; Packages
(setq-default indent-tabs-mode nil)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package sudo-edit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (setq evil-emacs-state-cursor `(,monokai-gray box))
  (setq evil-normal-state-cursor `(,monokai-red box))
  (setq evil-visual-state-cursor `(,monokai-orange box))
  (setq evil-insert-state-cursor `(,monokai-green bar))
  (setq evil-replace-state-cursor `(,monokai-blue bar))
  (setq evil-operator-state-cursor `(,monokai-green hollow))
  (setq evil-motion-state-cursor `(,monokai-blue bar))
  (evil-ex-define-cmd "ll" 'flymake-show-project-diagnostics))

(use-package evil-escape
  :after evil
  :bind
  ("<escape>" . evil-escape))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-define-key
   "C-<SPC>" 'mode-line-other-buffer)
  (general-define-key
   :states 'normal
   "[ s" 'flymake-goto-prev-error
   "] s" 'flymake-goto-next-error))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))
  (global-undo-tree-mode 1))

(use-package telephone-line
  :config
  (set-face-attribute 'telephone-line-evil-emacs nil
		      :background monokai-gray)
  (set-face-attribute 'telephone-line-evil-normal nil
		      :background monokai-red)
  (set-face-attribute 'telephone-line-evil-visual nil
		      :background monokai-orange)
  (set-face-attribute 'telephone-line-evil-insert nil
		      :background monokai-green)
  (set-face-attribute 'telephone-line-evil-replace nil
		      :background monokai-blue)
  (set-face-attribute 'telephone-line-evil-operator nil
		      :background monokai-green)
  (set-face-attribute 'telephone-line-evil-motion nil
		      :background monokai-blue)
  (setq telephone-line-lhs
	'((evil . (telephone-line-evil-tag-segment))
	  (accent . (telephone-line-vc-segment
		     telephone-line-erc-modified-channels-segment
		     telephone-line-process-segment))
	  (nil . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
	'((nil . (telephone-line-misc-info-segment))
	  (accent . (telephone-line-major-mode-segment))
	  (evil . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-identity-right
	telephone-line-secondary-left-separator 'telephone-line-identity-right
	telephone-line-primary-right-separator 'telephone-line-identity-left
	telephone-line-secondary-right-separator 'telephone-line-identity-left)
  (setq telephone-line-height 28)
  (telephone-line-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-plus)
	  (t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package projectile
  :demand t
  :config
  (projectile-mode 1)
  :bind
  ("C-c p" . projectile-find-file))

(use-package counsel
  :demand t
  :config
  (counsel-mode 1)
  :bind
  ("C-c a s" . counsel-rg)
  ("C-c a r" . counsel-recentf))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode 1))

(use-package avy
  :bind
  ("C-c a a" . avy-goto-char-2))

(use-package company
  :hook (prog-mode text-mode)
  :bind (:map company-active-map
              ("<return>" . company-complete-selection)))

(use-package company-box
  :hook company-mode)

(use-package flycheck
  :config
  (setq-default flycheck-checker-error-threshold 10000)
  (setq-default flycheck-disabled-checkers '(rust-cargo rust rust-clippy))
  (global-flycheck-mode 1))

(use-package flycheck-inline
  :config
  (global-flycheck-inline-mode 1))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package rainbow-delimiters
  :hook (prog-mode))

(use-package magit
  :demand
  :commands (magit-status)
  :bind (("C-c a g" . magit-status)))

(use-package which-key
  :config
  (which-key-mode))

(use-package diff-hl
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (flycheck-mode . diff-hl-flydiff-mode))
  :config
  (global-diff-hl-mode 1))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq-default rust-format-on-save t)
  (setq-default rust-format-show-buffer nil))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package yasnippet)

(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (wgsl-mode . eglot-ensure))
  :bind (("C-c a ." . eglot-code-actions)
         ("C-c a R" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs
               '(rust-mode
                 . ("rust-analyzer" :initializationOptions
                    (:checkOnSave (:extraArgs ["--target-dir" "/tmp/emacs-rust-analyzer"])))))
  (add-to-list 'eglot-server-programs
               '(wgsl-mode
                 . ("wgsl_analyzer"))))

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode)
  :config (customize-set-variable 'flymake-popon-method 'popon))

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.frag\\'" "\\.vert\\'"))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package rjsx-mode
  :mode "\\.jsx\\'")

(use-package js2-mode
  :mode ("\\.js\\'" "\\.es6\\'"))

(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'" "\\.cshtml\\'"))

(use-package lua-mode
  :mode ("\\.lua\\'" "\\.p8\\'"))

(use-package protobuf-mode
  :mode ("\\.proto'"))

(use-package dockerfile-mode
  :mode ("Dockerfile'"))

(use-package wgsl-mode
  :mode "\\.wgsl\\'")

(use-package systemd)

(use-package csharp-mode
  :mode "\\.cs'")

(use-package keychain-environment
  :config (keychain-refresh-environment))

(use-package ligature-pragmatapro
  :config (ligature-pragmatapro-setup))

(use-package ligature
  :config (global-ligature-mode 1))

(add-hook 'text-mode-hook 'flyspell-mode)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flymake-popon flymake-cursor eldoc-box ligature-pragmatapro ligature yasnippet yaml-mode which-key wgsl-mode web-mode use-package undo-tree tree-sitter-langs tree-sitter-indent telephone-line systemd sudo-edit rust-mode rjsx-mode rainbow-delimiters protobuf-mode monokai-theme memoize magit-delta lua-mode lsp-ui keychain-environment helm-lsp glsl-mode flycheck-rust flycheck-inline exec-path-from-shell evil-magit evil-escape evil-collection eglot dockerfile-mode diminish diff-hl dash-functional dap-mode csharp-mode counsel-projectile company-lsp company-box cargo all-the-icons))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
