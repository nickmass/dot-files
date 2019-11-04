;;; .emacs -- NickMass' emacs config
;;; Commentary:
;;; Rust and web dev ahoy.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

(eval-when-compile
(require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Appearence
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-hl-line-mode 1)
(setq inhibit-startup-screen 1)

(set-face-attribute 'default nil
		:family "Pragmata Pro"
		:height 160
		:weight 'bold)

;; Behavior
(server-start)
(setq-default js-indent-level 2)

;; Store backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(show-paren-mode 1)

(setq split-height-threshold 160)
(setq-default show-trailing-whitespace t)
(setq-default display-line-numbers 'relative)
(setq-default auto-hscroll-mode 'current-line)
(set-default 'truncate-lines 1)
(setq-default truncate-partial-width-windows 1)

;; Shift+Arrows to navigate windows
(windmove-default-keybindings)

(global-set-key (kbd "C-<SPC>") 'mode-line-other-buffer)

;; Persist hardlinks to files
(setq-default backup-by-copying t)

;; Packages
(setq use-package-always-ensure t)

(setq-default lsp-rust-server 'rust-analyzer)

(setq-default indent-tabs-mode nil)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (setq evil-emacs-state-cursor `(,monokai-gray box))
  (setq evil-normal-state-cursor `(,monokai-red box))
  (setq evil-visual-state-cursor `(,monokai-orange box))
  (setq evil-insert-state-cursor `(,monokai-green bar))
  (setq evil-replace-state-cursor `(,monokai-blue bar))
  (setq evil-operator-state-cursor `(,monokai-green hollow))
  (setq evil-motion-state-cursor `(,monokai-blue bar)))

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

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff 1)
  (with-no-warnings (global-undo-tree-mode 1)))

(use-package telephone-line
  :if window-system
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
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-plus)
	  (t . ivy--regex-plus))))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind
  ("C-c a s" . counsel-rg)
  ("C-c a f" . counsel-find-file)
  ("C-c a r" . counsel-recentf)
  ("C-c a d" . ivy-switch-buffer)
  ("C-c a b" . ivy-switch-buffer))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode 1))

(use-package avy
  :bind
  ("C-c a a" . avy-goto-char-2))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))

(use-package flycheck
  :config (global-flycheck-mode 1))

(use-package exec-path-from-shell
  :if window-system
  :init (exec-path-from-shell-initialize))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package magit
  :demand
  :commands (magit-status)
  :bind (("C-c a g" . magit-status)))

(use-package evil-magit
  :after (evil magit))

(use-package which-key
  :config
  (which-key-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'flycheck-mode-hook 'diff-hl-flydiff-mode))

(use-package rust-mode
  :mode ("\\.rs\\'")
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'cargo-process-mode-hook (lambda ()
                                       (setq truncate-lines nil)
                                       (setq truncate-partial-width-windows nil))))

(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq-default lsp-ui-sideline-show-symbol nil))
(use-package company-lsp
  :commands company-lsp)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package yasnippet)

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.frag\\'" "\\.vert\\'"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :config (flyspell-mode 1)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

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

(use-package csharp-mode
  :mode ("\\.cs'"))

(use-package keychain-environment
  :config (keychain-refresh-environment))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat (make-list (- (length (car s)) 1) (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"   #XE380)
            ("[DEBUG]"   #XE381)
            ("[INFO]"    #XE382)
            ("[WARN]"    #XE383)
            ("[WARNING]" #XE384)
            ("[ERR]"     #XE385)
            ("[FATAL]"   #XE386)
            ("[TRACE]"   #XE387)
            ("[FIXME]"   #XE388)
            ("[TODO]"    #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            ("!!"        #XE900)
            ("!="        #XE901)
            ("!=="       #XE902)
            ("!!!"       #XE903)
            ("!≡"        #XE904)
            ("!≡≡"       #XE905)
            ("!>"        #XE906)
            ("#("        #XE920)
            ("#_"        #XE921)
            ("#{"        #XE922)
            ("#?"        #XE923)
            ("#>"        #XE924)
            ("##"        #XE925)
            ("%="        #XE930)
            ("%>"        #XE931)
            ("<~"        #XE932)
            ("&%"        #XE940)
            ("&&"        #XE941)
            ("&*"        #XE942)
            ("&+"        #XE943)
            ("&-"        #XE944)
            ("&/"        #XE945)
            ("&="        #XE946)
            ("&&&"       #XE947)
            ("&>"        #XE948)
            ("***"       #XE960)
            ("*="        #XE961)
            ("*/"        #XE962)
            ("*>"        #XE963)
            ("++"        #XE970)
            ("+++"       #XE971)
            ("+="        #XE972)
            ("+>"        #XE973)
            ("++="       #XE974)
            ("--"        #XE980)
            ("-<"        #XE981)
            ("-<<"       #XE982)
            ("-="        #XE983)
            ("->"        #XE984)
            ("->>"       #XE985)
            ("---"       #XE986)
            ("-->"       #XE987)
            ("-+-"       #XE988)
            ("-\\/"      #XE989)
            (".."        #XE990)
            ("..."       #XE991)
            ("..<"       #XE992)
            (".>"        #XE993)
            (".~"        #XE994)
            (".="        #XE995)
            ("/*"        #XE9A0)
            ("//"        #XE9A1)
            ("/>"        #XE9A2)
            ("/="        #XE9A3)
            ("/=="       #XE9A4)
            ("///"       #XE9A5)
            ("/**"       #XE9A6)
            ("::"        #XE9B0)
            (":="        #XE9B1)
            (":≡"        #XE9B2)
            (":>"        #XE9B3)
            (":=>"       #XE9B4)
            (":("        #XE9B5)
            (":-("       #XE9B6)
            (":)"        #XE9B7)
            (":-)"       #XE9B8)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<$>"       #XE9C0)
            ("<*"        #XE9C1)
            ("<*>"       #XE9C2)
            ("<+>"       #XE9C3)
            ("<-"        #XE9C4)
            ("<<"        #XE9C5)
            ("<<<"       #XE9C6)
            ("<<="       #XE9C7)
            ("<="        #XE9C8)
            ("<=>"       #XE9C9)
            ("<>"        #XE9CA)
            ("<|>"       #XE9CB)
            ("<<-"       #XE9CC)
            ("<|"        #XE9CD)
            ("<=<"       #XE9CE)
            ("<~"        #XE9CF)
            ("<~~"       #XE9D0)
            ("<<~"       #XE9D1)
            ("<$"        #XE9D2)
            ("<+"        #XE9D3)
            ("<!>"       #XE9D4)
            ("<@>"       #XE9D5)
            ("<#>"       #XE9D6)
            ("<%>"       #XE9D7)
            ("<^>"       #XE9D8)
            ("<&>"       #XE9D9)
            ("<?>"       #XE9DA)
            ("<.>"       #XE9DB)
            ("</>"       #XE9DC)
            ("<\\>"      #XE9DD)
            ("<\">"      #XE9DE)
            ("<:>"       #XE9DF)
            ("<~>"       #XE9E0)
            ("<**>"      #XE9E1)
            ("<<^"       #XE9E2)
            ("<!"        #XE9E3)
            ("<@"        #XE9E4)
            ("<#"        #XE9E5)
            ("<%"        #XE9E6)
            ("<^"        #XE9E7)
            ("<&"        #XE9E8)
            ("<?"        #XE9E9)
            ("<."        #XE9EA)
            ("</"        #XE9EB)
            ("<\\"       #XE9EC)
            ("<\""       #XE9ED)
            ("<:"        #XE9EE)
            ("<->"       #XE9EF)
            ("<!--"      #XE9F0)
            ("<--"       #XE9F1)
            ("<~<"       #XE9F2)
            ("<==>"      #XE9F3)
            ("==<"       #XEA00)
            ("=="        #XEA01)
            ("==="       #XEA02)
            ("==>"       #XEA03)
            ("=>"        #XEA04)
            ("=~"        #XEA05)
            ("=>>"       #XEA06)
            ("=/="       #XEA07)
            ("≡≡"        #XEA10)
            ("≡≡≡"       #XEA11)
            ("≡:≡"       #XEA12)
            (">-"        #XEA20)
            (">="        #XEA21)
            (">>"        #XEA22)
            (">>-"       #XEA23)
            (">=="       #XEA24)
            (">>>"       #XEA25)
            (">=>"       #XEA26)
            (">>^"       #XEA27)
            ("??"        #XEA40)
            ("?~"        #XEA41)
            ("?="        #XEA42)
            ("?>"        #XEA43)
            ("???"       #XEA44)
            ("^="        #XEA48)
            ("^."        #XEA49)
            ("^?"        #XEA4A)
            ("^.."       #XEA4B)
            ("^<<"       #XEA4C)
            ("^>>"       #XEA4D)
            ("^>"        #XEA4E)
            ("\\\\"      #XEA50)
            ("\\>"       #XEA51)
            ("\\/-"      #XEA52)
            ("@>"        #XEA57)
            ("|="        #XEA60)
            ("||"        #XEA61)
            ("|>"        #XEA62)
            ("|||"       #XEA63)
            ("|+|"       #XEA64)
            ("|->"       #XEA65)
            ("|-->"      #XEA66)
            ("|=>"       #XEA67)
            ("|==>"      #XEA68)
            ("~="        #XEA70)
            ("~>"        #XEA71)
            ("~~>"       #XEA72)
            ("~>>"       #XEA73)
            ("\">"       #XEA90))))

(defun add-pragmatapro-prettify-symbols-alist ()
  "Add pragmatapro ligs to list."
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

(if (window-system)
    (add-hook 'prog-mode-hook
              #'add-pragmatapro-prettify-symbols-alist))

(global-prettify-symbols-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(load "~/.emacs.d/ra-emacs-lsp.el")

;;; .emacs ends here
