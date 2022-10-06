;;;; UI --------------- 
;; General Interface
(setq inhibit-startup-message t)
(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable top toolbar
(tooltip-mode -1)     ; Disable tooltips
(menu-bar-mode -1)    ; Disable top menubar
(set-fringe-mode 10)  ; Add some padding
(setq visible-bell t)  ; Flash instead of klaxon

;; Fonts
(defvar z/default-font-size 160)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height z/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height z/default-font-size)
(set-face-attribute 'variable-pitch nil :font "EtBembo" :height z/default-font-size :weight 'regular)

;; Line Numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (dispaly-line-numbers-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;;; PACKAGES --------------- 
;;; Initialize Package Sources
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; use-package
;; :init :: lisp to run pre load hook
;; :config :: lisp to run post load hook


;; # All The Icons
;; NOTE: when setting up on new machine, you need to run `M-x all-the-icons-install-fonts'
;; Source:
(use-package all-the-icons)

;; # Command Log Mode
;; Echo key binds in a separate buffer, for demos and such
;; Source:
(use-package command-log-mode)

;; # Doom Modeline
;; Fancier mode line with icons
;; Source:
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; # Dumb Jump
;; Go to definition for like every programming language
;; Source: https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :after hydra
  :config
  ;; Enable xref backend, https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Add dumb-jump hydra
  (defhydra hydra-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "go")
    ("o" dumb-jump-go-other-window "other window")
    ("e" dumb-jump-go-prefer-external "go external")
    ("x" dumb-jump-go-prefer-external-other-window "go external other window")
    ("i" dumb-jump-go-prompt "prompt")
    ("l" dumb-jump-quick-look "quick look")
    ("b" dumb-jump-back "back"))
  (z/leader-keys
    "cg" '(hydra-jump/body :which-key "go to definition"))
  )

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
         :config
         (ivy-mode 1))

;; # Solaire-Mode
;; NOTE: must be turned on before doom-themes
;; source: https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  :hook (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  :hook (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  :hook (ediff-prepare-buffer . solaire-mode)
  ;; Highlight the minibuffer when it is activated:
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; The bright and dark background colors are automatically swapped the first 
  ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
  ;; `solaire-default-face` faces are swapped. This is done because the colors 
  ;; are usually the wrong way around. If you don't want this, you can disable it:
  (setq solaire-mode-auto-swap-bg nil)

  (solaire-global-mode +1))

(use-package doom-themes
  :config
  ;; Global settings
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; Initial theme on start
  (load-theme 'doom-one t)
  ;; Flash modeline instead of klaxon or /!\ icon flash
  (doom-themes-visual-bell-config)
  ;; Atom-like file explorer (NOTE: install all-the-icons to use)
  (doom-themes-neotree-config)
  ;; Improved Org Mode fontification
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; # Which-Key
;; Display options to complete the rest of a keybind (e.g. type `C-h' and wait to see all the help commands, like `f' or `v')
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10)
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  (which-key-setup-side-window-bottom))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; # Helpful
;; Better help documentation and functions
(use-package helpful
  :custom
  ;; Replace counsel's describe-(function|variable) functions with helpful's
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; Keep counsel's minibuffer descriptions over helpful's
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; # General
;; Conveniently bind keys, instead of `global-set-key' and `define-key'
;; Source: https://github.com/noctuid/general.el
(use-package general
  :config
  (general-create-definer z/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (z/leader-keys
    ;; Prefixes
    ;;"" '( :which-key "")
    ;; a :: apps

    ;; b :: buffer
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bj" '(next-buffer :which-key "next buffer")
    "bk" '(previous-buffer :which-key "previous buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
    "bm" '(bookmark-set :which-key "set bookmark")
    "bM" '(bookmark-delete :which-key "delete bookmark")
    "bn" '(evil-buffer-new :which-key "new buffer")
    ;;   bs :: save?
    ;;   bS :: save all? save as root?
    ;;   bx :: open scratch buffer? pop up scratch buffer?

    ;; c :: code
    "c" '(:ignore t :which-key "code")
    "ce" '(eval-last-sexp :which-key "eval expression")
    "cE" '(eval-buffer :which-key "eval buffer")

    ;; f :: file
    "f" '(:ignore t :which-key "file")
    ;;"fd" '(+default/dired :which-key "find directory")
    ;;"fe" '(find-file-in-emacsd :which-key "find in emacs.d")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "fS" '(write-buffer :which-key "save file as...")

    ;; g :: git

    ;; h :: help
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hF" '(describe-face :which-key "describe face")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")
    "hp" '(describe-package :which-key "describe package")
    "hv" '(describe-variable :which-key "describe variable")
    
    ;; i :: insert
    ;; m :: mode-specific

    ;; n :: notes
    "n" '(:ignore t :which-key "notes")
    "na" '(org-agenda :which-key "org agenda")
    ;;"nc" '(+org/toggle-last-clock :which-key "toggle last org-clock")
    ;;"nC" '(org-clock-cancel :which-key "cancel org-clock")
    ;;"nd" '(deft :which-key "open deft")
    "nt" '(org-todo-list :which-key "org todos")

    ;; o :: open
    "o" '(:ignore t :which-key "open")
    "oe" '(eshell :which-key "eshell")
    ;;   f :: open in finder?
    ;;   o :: open at point
    "ot" '(vterm :which-key "terminal")

    ;; p :: project

    ;; q :: quit/session
    "q" '(:ignore t :which-key "quit/session")
    "qq" '(save-buffers-kill-emacs :which-key "quit")
    "qQ" '(evil-quit-all-with-error-code :which-key "quit without save")

    ;; t :: toggle
    "t" '(:ignore t :which-key "toggle")
    "tt" '(counsel-load-theme :which-key "choose theme")

    "u" '(universal-argument :which-key "universal argument")
    
    ;; w :: window
    "w" '(:ignore t :which-key "window")
    "wd" '(evil-window-close :which-key "close window")
    "wD" '(delete-other-windows :which-key "close other windows")
    "ww" '(evil-window-next :which-key "next window")
    "wn" '(evil-window-next :which-key "next window")
    "wp" '(evil-window-previous :which-key "previous window")

    "wh" '(evil-window-left :which-key "move cursor left")
    "wH" '(evil-window-move-far-left :which-key "move window left")
    "wj" '(evil-window-down :which-key "move cursor down")
    "wJ" '(evil-window-move-very-bottom :which-key "move window down")
    "wk" '(evil-window-up :which-key "move cursor up")
    "wK" '(evil-window-move-very-top :which-key "move window up")
    "wl" '(evil-window-right :which-key "move cursor right")
    "wL" '(evil-window-move-far-right :which-key "move window right")

    "ws" '(evil-window-split :which-key "horizontal split")
    "w-" '(evil-window-split :which-key "horizontal split")
    "wv" '(evil-window-vsplit :which-key "vertical split")
    "w\\" '(evil-window-vsplit :which-key "vertical split")
    "w=" '(balance-windows :which-key "balance windows")
    ))

;; # Evil
;; Use vim-like modes and keybindings
;; Source: https://github.com/emacs-evil/evil
(use-package evil
  :init
  ;; Evil looks for these variables /BEFORE/ loading so that it can set bindings accordingly
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width 2)
  ;; Always use "visual line" not "lexical line" motions (think vim's `g j' and `g k' motions)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  ;;(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; # Evil Collection
;; Evil bindings for everything that Evil doesn't cover by default
;; Source: https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; # Evil Escape
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

;; # Forge
;; Connection to GitHub?
;; NOTE: make sure to configure GH token before using
;; Source: https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; (use-package forge)

;; # Hydra
;; Create "hydra" commands, lots of short transient keybindings branching from common prefix
;; Source: https://github.com/abo-abo/hydra 
(use-package hydra
  :after general
  :config
  (defhydra hydra-text-size (:timeout 4)
    "change font size"
    ("j" text-scale-decrease "down")
    ("k" text-scale-increase "up")
    ("q" nil "quit" :exit t))

  (defhydra hydra-window-size (:timeout 4)
    "change window size"
    ("j" evil-window-decrease-height "decrease height")
    ("k" evil-window-increase-height "increase height")
    ("h" evil-window-decrease-width "decrease width")
    ("l" evil-window-increase-width "increase width")
    ("q" nil "quit" :exit t))

  ;; Add hydras to general.el keybinds
  (z/leader-keys
    "tf" '(hydra-text-size/body :which-key "change font size")
    "wc" '(hydra-window-size/body :which-key "change window size"))
  )

;; # Projectile
;; Work with projects
;; Source:
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; # Counsel Projectile
;; Use Counsel for Projectile commands
;; Source: 
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; # Magit
;; Git within Emacs
;; Source:
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; # Org Mode
;; Organize my everything
;; Source:
(defun z/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org mode font configuration
(defun z/org-mode-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "EtBembo" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . z/org-mode-setup)
  :config
  (setq org-clock-persist 'history
	org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (org-clock-persistence-insinuate)
  (z/org-mode-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●")))

;; # Undo Tree
;; Store undo history in a tree like structure, easier than vanilla emacs undo behavior
;; NOTE: if this gets flaky, try undo-fu instead. i've heard it's more stable, but this has nifty features.
;; Source:
(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-auto-save-history t
	undo-tree-enable-undo-in-region t
	undo-limit 800000
	undo-strong-limit 12000000
	undo-outer-limit 120000000)
  (global-undo-tree-mode))

;; # Visual Fill Column
;; Add padding to let things breath
;; Source: https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :config
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  :hook visual-line-mode)

;; # Vterm
;; Fully-fledged terminal emulator integration based on libvterm, a compiled library in C
;; Source: https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t)

;; # Web Mode
;; Major mode for web templating languages
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" default))
 '(package-selected-packages
   '(dumb-jump vterm web-mode visual-fill-column org-bullets evil-escape undo-tree evil-magit magit counsel-projectile projectile hydra evil-collection evil general doom-modeline ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
