;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; use `SPACE f e R' to re-exucute spacemacs init

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/code/dotfiles/emacs.d/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     ;; personal
     auto-completion
     better-defaults
     colors
     ;; (git :variables
     ;;      git-gutter-use-fringe t)
     html
     javascript
     lua
     markdown
     org
     osx
     php
     python
     restclient
     shell-scripts
     syntax-checking
     themes-megapack
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; A list of additional packages that will be installed and loaded
   dotspacemacs-additional-packages '(base16-theme)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; set the path variable (mostly for annoying tern fix)
  ;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(base16-bespin-dark
                         monokai
                         solarized-dark
                         solarized-light
                         spacegray
                         material-dark
                         material-light
                         gotham
                         zenburn
                         sanityinc-tomorrow-night
                         darktooth)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("Source Code Pro"
    dotspacemacs-default-font '("Menlo"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 1.0
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;;
   ;; NEW CHANGES
   ;;
   ;; escape with f-j instead of <ESC>
   evil-escape-key-sequence "jk"
   evil-escape-delay 0.3
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()
    "Configuration function.
    This function is called at the very end of Spacemacs initialization after
    layers configuration."

    ;;;; GENERAL SETTINGS ;;;;

    ;; homebrew installed packages are here:
    (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
        (normal-top-level-add-subdirs-to-load-path))

    ;; themes
    (set-mouse-color "white")
    ;; (set-mouse-color "black")

    ;; power-line
    (setq powerline-default-separator 'arrow)

    ;; switching command and option keys in osx
    ;; (setq mac-option-modifier 'super)
    ;; (setq mac-command-modifier 'meta)

    ;; enable C-x k to close emacsclient
    (add-hook 'server-switch-hook
        (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

    ;; enable C-n movement to add new lines
    (setq next-line-add-newlines t)

    ;; enable copy-paste
    (setq x-select-enable-clipboard t)

    ;; Set php files to open in Web-Mode by default
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))


    ;;;; PERSONAL KEYBINDINGS ;;;;

    ;; create the prefix for all bindings
    (spacemacs/declare-prefix "o" "personal-prefix")

    ;; open default org file
    (defun open-org-inbox ()
      "Opens the default org-mode file in a new buffer."
      (interactive)
      (find-file "~/org/inbox.txt"))
    (evil-leader/set-key "oo" 'open-org-inbox)


    ;;;; ORG-MODE SETTINGS ;;;;

    ;; creating org workflow states
    (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "HOLD" "DONE")))

    ;; setting todo keyword colors
    (setq org-todo-keyword-faces
          ;;'(("TODO" . (:foreground "#268bd2" :weight bold)) ("HOLD" . (:foreground "#268bd2" :weight bold)) ("NEXT" . (:foreground "#2aa198" :weight bold))))
          '(("NEXT" . (:foreground "#2aa198" :weight bold))))

    ;; david o'toole's org tutorial configuration
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)

    ;; set org-capture default file
    (setq org-default-notes-file "~/org/inbox.txt")

    ;; set org-capture keybinding
    (define-key global-map "\C-cc" 'org-capture)

    ;; set up org-capture templates
    (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                "* TODO %?\n  %i\n  %a")
        ("j" "JOURNAL" entry (file+datetree "~/org/journal.txt")
         "* %< %R >\n\n %?"
         :empty-lines 1)))

    ;; creating source code block insert function
    ;; found here http://wenshanren.org/?p=334
    (defun org-insert-src-block (src-code-type)
        "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
        (interactive
        (let ((src-code-types
                '("emacs-lisp" "python" "C" "shell" "java" "js" "clojure" "C++"
                  "css" "php" "R" "sass" "scss" "less" "sql" "awk" "latex"
                  "lisp" "org" "perl" "ruby" "scheme" "sqlite")))
            (list (ido-completing-read "Source code type: " src-code-types))))
        (progn
            (newline-and-indent)
            (insert (format "#+BEGIN_SRC %s\n" src-code-type))
            (newline-and-indent)
            (insert "#+END_SRC\n")
            (previous-line 2)
            (org-edit-src-code)))

    ;; keybindings to insert and edit source blocks in org-mode
    (define-key global-map "\C-cs" 'org-insert-src-block)
    (define-key global-map "\C-ce" 'org-edit-src-code)
    (evil-leader/set-key
        "Cs" 'org-insert-src-block
        "Ce" 'org-edit-src-code
        "C'" 'org-edit-special)

    ;; changing vim section navigation keybindings to heading navigation in org-mode
;    (evil-define-key ’normal org-mode-map "[[" ’org-backward-heading-same-level)
;    (evil-define-key ’normal org-mode-map "]]" ’org-forward-heading-same-level)
;    (evil-define-key ’normal org-mode-map "[]" ’org-goto)
;    (evil-define-key ’normal org-mode-map "][" ’outline-up-heading)

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" default)))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#49483E" t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
