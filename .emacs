(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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

(set-face-attribute 'default nil
		:family "Pragmata Pro"
		:height 160
		:weight 'bold)


;;Behavior

 ;;Store backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;Restore sessions on launch
(desktop-save-mode 1)

(show-paren-mode 1)

(setq show-trailing-whitespace t)

;; Shift+Arrows to navigate windows
(windmove-default-keybindings)

 ;; Packages
(setq use-package-always-ensure t)

(use-package base16-theme
  :config
  (load-theme 'base16-monokai t))

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursor '("#A6E22E" box))         ;; Green
  (setq evil-normal-state-cursor '("#F92672" box))        ;; Red
  (setq evil-visual-state-cursor '("#FD971F" box))        ;; Orange
  (setq evil-insert-state-cursor '("#A6E22E" bar))        ;; Green
  (setq evil-replace-state-cursor '("#66D9EF" bar))       ;; Blue
  (setq evil-operator-state-cursor '("#A6E22E" hollow)))  ;; Green

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
	  (t . ivy--regex-plus))))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind ("C-x f" . counsel-find-file))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :config (global-flycheck-mode 1))

(use-package exec-path-from-shell
  :if window-system
  :init (exec-path-from-shell-initialize))

(use-package relative-line-numbers
  :config
  (defun relative-line-numbers-custom-format (offset)
    (format "%3d" (abs offset)))
  (setq relative-line-numbers-format #'relative-line-numbers-custom-format)
  (global-relative-line-numbers-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(use-package rust-mode
  :mode ("\\.rs\\'" "\\.toml\\'"))

(use-package cargo
  :after rust-mode)

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package rjsx-mode
  :mode "\\.jsx\\'")

(use-package js2-mode
  :mode ("\\.js\\'" "\\.es6\\'"))

(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'"))

;; Enable ligatures without prettify-symbols

(provide 'add-pragmatapro-symbol-keywords)

(defconst pragmatapro-fontlock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ,(concat (list ?\C-i)
                                            (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(\\[ERROR\\]\\)"      #XE380)
            ("\\(\\[DEBUG\\]\\)"      #XE381)
            ("\\(\\[INFO\\]\\)"       #XE382)
            ("\\(\\[WARN\\]\\)"       #XE383)
            ("\\(\\[WARNING\\]\\)"    #XE384)
            ("\\(\\[ERR\\]\\)"        #XE385)
            ("\\(\\[FATAL\\]\\)"      #XE386)
            ("\\(\\[TRACE\\]\\)"      #XE387)
            ("\\(!!\\)"               #XE720)
            ("\\(!=\\)"               #XE721)
            ("\\(!==\\)"              #XE722)
            ("\\(!!!\\)"              #XE723)
            ("\\(!≡\\)"               #XE724)
            ("\\(!≡≡\\)"              #XE725)
            ("[^<]\\(!>\\)"           #XE726)
            ("\\(#(\\)"               #XE740)
            ("\\(#_\\)"               #XE741)
            ("\\(#{\\)"               #XE742)
            ("\\(#\\?\\)"             #XE743)
            ("[^<]\\(#>\\)"           #XE744)
            ("\\(##\\)"               #XE745)
            ("\\(%=\\)"               #XE750)
            ("[^<]\\(%>\\)"           #XE751)
            ("\\(&%\\)"               #XE760)
            ("\\(&&\\)"               #XE761)
            ("\\(&\\*\\)"             #XE762)
            ("\\(&\\+\\)"             #XE763)
            ("\\(&-\\)"               #XE764)
            ("\\(&/\\)"               #XE765)
            ("\\(&=\\)"               #XE766)
            ("\\(&&&\\)"              #XE767)
            ("[^<]\\(&>\\)"           #XE768)
            ("\\(\\*\\*\\*\\)"        #XE780)
            ("\\(\\*=\\)"             #XE781)
            ("\\(\\*/\\)"             #XE782)
            ("[^<]\\(\\*>\\)"         #XE783)
            ("\\(\\+\\+\\)"           #XE790)
            ("\\(\\+\\+\\+\\)"        #XE791)
            ("[^\\+]\\(\\+=\\)"       #XE792)
            ("[^<]\\(\\+>\\)"         #XE793)
            ("\\(\\+\\+=\\)"          #XE794)
            ("\\(--\\)"               #XE7A0)
            ("[^-]\\(-<\\)"           #XE7A1)
            ("\\(-<<\\)"              #XE7A2)
            ("\\(-=\\)"               #XE7A3)
            ("[^|]\\(->\\)"           #XE7A4)
            ("[^|]\\(->>\\)"          #XE7A5)
            ("\\(---\\)"              #XE7A6)
            ("\\(-->\\)"              #XE7A7)
            ("\\(-\\+-\\)"            #XE7A8)
            ("\\(-\\\\/\\)"           #XE7A9)
            ("[^\\^]\\(\\.\\.\\)"     #XE7B0)
            ("\\(\\.\\.\\.\\)"        #XE7B1)
            ("\\(\\.\\.<\\)"          #XE7B2)
            ("\\(\\.>\\)"             #XE7B3)
            ("\\(\\.~\\)"             #XE7B4)
            ("\\(\\.=\\)"             #XE7B5)
            ("\\(/\\*\\)"             #XE7C0)
            ("\\(//\\)"               #XE7C1)
            ("[^<]\\(/>\\)"           #XE7C2)
            ("[^=]\\(/=\\)"           #XE7C3)
            ("\\(/==\\)"              #XE7C4)
            ("\\(///\\)"              #XE7C5)
            ("\\(/\\*\\*\\)"          #XE7C6)
            ("\\(::\\)"               #XE7D0)
            ("\\(:=\\)"               #XE7D1)
            ("[^≡]\\(:≡\\)"           #XE7D2)
            ("\\(:>\\)"               #XE7D3)
            ("\\(:=>\\)"              #XE7D4)
            ("\\(<\\*\\)"             #XE7E1)
            ("\\(<\\*>\\)"            #XE7E2)
            ("[^<]\\(<-\\)"           #XE7E4)
            ("[^-]\\(<<\\)"           #XE7E5)
            ("\\(<<<\\)"              #XE7E6)
            ("\\(<<=\\)"              #XE7E7)
            ("[^<]\\(<=\\)"           #XE7E8)
            ("\\(<=>\\)"              #XE7E9)
            ("\\(<>\\)"               #XE7EA)
            ("\\(<<-\\)"              #XE7EC)
            ("\\(<|\\)"               #XE7ED)
            ("\\(<|>\\)"              #XE7EB)
            ("\\(<=<\\)"              #XE7EE)
            ("[^<]\\(<~\\)"           #XE7EF)
            ("\\(<~~\\)"              #XE7F0)
            ("\\(<<~\\)"              #XE7F1)
            ("\\(<\\$\\)"             #XE7F2)
            ("\\(<\\$>\\)"            #XE7E0)
            ("\\(<\\+\\)"             #XE7F3)
            ("\\(<\\+>\\)"            #XE7E3)
            ("\\(<~>\\)"              #XE800)
            ("\\(<\\*\\*>\\)"         #XE801)
            ("\\(<<\\^\\)"            #XE802)
            ("\\(<!\\)"               #XE803)
            ("\\(<!>\\)"              #XE7F4)
            ("\\(<@\\)"               #XE804)
            ("\\(<#\\)"               #XE805)
            ("\\(<#>\\)"              #XE7F6)
            ("\\(<%\\)"               #XE806)
            ("\\(<%>\\)"              #XE7F7)
            ("[^<]\\(<\\^\\)"         #XE807)
            ("\\(<&\\)"               #XE808)
            ("\\(<&>\\)"              #XE7F9)
            ("\\(<\\?\\)"             #XE809)
            ("\\(<\\.\\)"             #XE80A)
            ("\\(<\\.>\\)"            #XE7FB)
            ("\\(</\\)"               #XE80B)
  ;;        ("\\(</>\\)"              #XE7FC)
            ("\\(<\\\\\\)"            #XE80C)
            ("\\(<\"\\)"              #XE80D)
            ("\\(<\">\\)"             #XE7FE)
            ("\\(<:\\)"               #XE80E)
            ("\\(<:>\\)"              #XE7FF)
            ("\\(<->\\)"              #XE80F)
            ("\\(<!--\\)"             #XE810)
            ("\\(<--\\)"              #XE811)
            ("\\(<~<\\)"              #XE812)
            ("\\(<==>\\)"             #XE813)
            ("\\(==<\\)"              #XE820)
            ("[^/!<=>]\\(==\\)[^><=]" #XE821)
            ("\\(===\\)"              #XE822)
            ("[^<]\\(==>\\)"          #XE823)
            ("[^=:<]\\(=>\\)"         #XE824)
            ("\\(=~\\)"               #XE825)
            ("\\(=>>\\)"              #XE826)
            ("\\(=/=\\)"              #XE827)
            ("[^!]\\(≡≡\\)"           #XE830)
            ("\\(≡≡≡\\)"              #XE831)
            ("\\(≡:≡\\)"              #XE832)
            ("[^>]\\(>-\\)"           #XE840)
            ("\\(>=\\)"               #XE841)
            ("[^=-]\\(>>\\)"          #XE842)
            ("\\(>>-\\)"              #XE843)
            ("\\(>==\\)"              #XE844)
            ("\\(>>>\\)"              #XE845)
            ("\\(>=>\\)"              #XE846)
            ("\\(>>\\^\\)"            #XE847)
            ("\\(\\?\\?\\)"           #XE860)
            ("\\(\\?~\\)"             #XE861)
            ("\\(\\?=\\)"             #XE862)
            ("\\(\\?>\\)"             #XE863)
            ("\\(<\\?>\\)"            #XE7FA)
            ("\\(\\?\\?\\?\\)"        #XE864)
            ("\\(\\^=\\)"             #XE868)
            ("\\(\\^\\.\\)"           #XE869)
            ("\\(\\^\\?\\)"           #XE86A)
            ("\\(\\^\\.\\.\\)"        #XE86B)
            ("\\(\\^<<\\)"            #XE86C)
            ("\\(\\^>\\)"             #XE86E)
            ("\\(\\^>>\\)"            #XE86D)
            ("\\(<\\^>\\)"            #XE7F8)
            ("[^\\\\]\\(\\\\\\\\\\)"  #XE870)
            ("[^<]\\(\\\\>\\)"        #XE871)
            ("\\(<\\\\>\\)"           #XE7FD)
            ("\\(\\\\/-\\)"           #XE872)
            ("\\(@>\\)"               #XE877)
            ("\\(<@>\\)"              #XE7F5)
            ("\\(|=\\)"               #XE880)
            ("\\(||\\)"               #XE881)
            ("[^<]\\(|>\\)"           #XE882)
            ("\\(|||\\)"              #XE883)
            ("\\(|\\+|\\)"            #XE884)
            ("\\(|->\\)"              #XE885)
            ("\\(|-->\\)"             #XE886)
            ("\\(|=>\\)"              #XE887)
            ("\\(|==>\\)"             #XE888)
            ("\\(~=\\)"               #XE890)
            ("[^~<]\\(~>\\)"          #XE891)
            ("\\(~~>\\)"              #XE892)
            ("\\(~>>\\)"              #XE893)
            ("[^<]\\(\">\\)"          #XE8B0))))

(defun add-pragmatapro-symbol-keywords ()
  (font-lock-add-keywords nil pragmatapro-fontlock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-pragmatapro-symbol-keywords)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel magit company-mode use-package racer ivy flycheck-rust evil cargo base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
