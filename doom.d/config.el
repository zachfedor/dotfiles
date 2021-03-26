;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zach Fedor"
      user-mail-address "zachfedor@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'medium)
      ;; doom-serif-font (font-spec :family "EtBembo" :size 18)
      doom-variable-pitch-font (font-spec :family "EtBembo" :size 18)
      ;; doom-unicode-font (font-spec :family "Source Code Pro" :size 18)
      doom-big-font (font-spec :family "Fira Code Medium" :size 24))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes/")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; source: https://www.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
;; dotfiles: https://github.com/psamim/dotfiles/blob/master/doom/config.el#L59
;; (setq mixed-pitch-fixed-pitch-faces
;;      (quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))


;; Interface
(setq visible-bell t
      ring-bell-function nil)

;; Start fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(top . 18))
(add-to-list 'default-frame-alist '(left . 18))
(add-to-list 'default-frame-alist '(width . 170))
(add-to-list 'default-frame-alist '(height . 45))


;;; Org mode
(after! org
  (setq org-deadline-warning-days 7
        org-ellipsis " ▾ "
        org-list-demote-modify-bullet '(("1." . "a."))
        org-log-done t
        org-log-into-drawer t
        org-tags-column -80))

(defun add-created-timestamp-property ()
  "Add CREATED property to current item with timestamp"
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))
(add-hook 'org-capture-before-finalize-hook 'add-created-timestamp-property)

(setq-hook! org-mode
  org-fontify-done-headline t
  org-fontify-quote-and-verse-blocks t
  org-fontify-whole-heading-line t
  org-image-actual-width '(800))

;; Allow variable pitch fonts in Org mode, set different cursor
(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(setq mixed-pitch-variable-pitch-cursor nil)
(add-hook! org-mode :append
           #'variable-pitch-mode
           #'visual-line-mode)

;; Turn off spelling and completion until called
(add-hook! org-mode (flyspell-mode 0) (company-mode 0))

;; Open .txt files in Org mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(defun z/org-downcase-keywords ()
  "Convert Org mode keywords and block identifiers to lower case in current buffer.

Example: #+TITLE -> #+title
         #+BEGIN_SRC -> #+begin_src

Source: https://scripter.co/org-keywords-lower-case/"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Downcased %d matches" count))))

(defun z/org-downcase-properties ()
  "Convert Org mode property drawers to lowercase in current buffer.

TODO: there's definitely a better way to accomplish this...

Example: :PROPERTIES: -> :properties:

Inspired by above z/org-downcase-keywords function"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward "\\(?1:\\:\\(?:PROPERTIES\\|LOGBOOK\\|END\\)\\:\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil ))
      (message "Downcased %d matches" count))))

;;; Deft
(use-package! deft
  :config (setq deft-default-extension "txt"
                deft-directory "~/Dropbox/notes"
                deft-extensions '("txt" "org" "md") ;; the first item will be used when creating new files
                deft-file-naming-rules '((nospace . "-")
                                         (case-fn . downcase)) ;; rules to convert filter string to filename
                deft-org-mode-title-prefix t ;; add Org mode #+TITLE prefix to new files
                deft-recursive t ;; search in sub-directories too
                deft-use-filename-as-title nil  ;; use first line of file as Deft title
                deft-use-filter-string-for-filename t)) ;; use filter string to create new file with readable filename, see `deft-file-naming-rules'

;;; Org-Roam
;; (use-package! org-roam
;;   :config (setq org-roam-directory "~/Dropbox/notebox")
;;           (org-roam-db-build-cache))

;;; Org-Superstar
(use-package! org-superstar
  :config (setq org-superstar-cycle-headline-bullets nil  ;; don't cycle, just repeat the last bullet in list
                org-superstar-headline-bullets-list '(?◉ ?○ ?▷)))

;; Keybindings
(map!
 :desc "Lowercase word at point" "M-u" #'downcase-word
 :desc "Uppercase word at point" "M-U" #'upcase-word
 :desc "Eval org src block and print results" "C-c C-c" #'org-babel-execute-src-block)

;;; Indentation
;;
;; TODO this didn't work exactly as expected. Org mode doc had multiple indent widths:
;; quote block and paragraph used 4, but list used 1. both with hitting TAB in insert
;; mode and using >> operator in normal mode. no idea what's happening...
;;
;; Source: https://dougie.io/emacs/indentation/
;; Set personal tab width and create functions to switch between tabs/spaces
;; (setq custom-tab-width 2)

;; (defun disable-tabs ()
;;   "Stop using tab characters for indentation"
;;   (setq indent-tabs-mode nil))

;; (defun enable-tabs ()
;;   "Start using tab characters for indentation"
;;   (local-set-key (KBD "TAB") 'tab-to-tab-stop)
;;   (setq indent-tabs-mode t)
;;   (setq tab-width custom-tab-width))

;; Hook into major modes to switch tab/space usage
;; (add-hook 'prog-mode-hook 'disable-tabs)
;; Configure other tools to use custom width
;; (setq evil-shift-width custom-tab-width)

;; TODO setup invisible character symbols
;; Source: http://ergoemacs.org/emacs/whitespace-mode.html


;;; Literate Config
;; source: https://tecosaur.github.io/emacs-config/config.html
;; Turning on the =:config literate= module in =init.el= allows converting this
;; file into =config.org= for a literate config file. TODO
(setq-default
 delete-by-moving-to-trash t            ; Delete files to trash
 x-stretch-cursor t                    ; Stretch cursor to contain glyph
 major-mode 'org-mode)


(setq
 undo-limit 80000000                    ; Raise undo history 80Mb
 evil-want-fine-undo t                  ; Granular insert-mode changes
 auto-save-default t                    ; No worries
 truncate-string-ellipsis "…"           ; Unicode ellipses for space efficiency
 display-line-numbers-type 'relative)   ; vim-like relative line numbers for easy navigation

(setq evil-escape-delay 0.20)

(display-time-mode 1)                   ; Enable mode-line clock
;(if (equal "Battery status not available"
;           (battery))
;    (display-battery-mode 1))           ; Enable battery status on laptop

;; Windows
;; ask for buffer when splitting windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
;; add previews
(setq +ivy-buffer-preview t)
;; add layout rotation, like tmux =C-b SPC=
(map! :map evil-window-map
      "SPC" #'rotate-layout)

;; Theme
(delq! t custom-theme-load-path)        ; remove default themes
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange")) ; replace scary red save icon on modified files
(defun doom-modeline-conditional-buffer-encoding ()
  "File encoding is expected to be LF UTF-8, so only show unexpected encoding in modeline"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

;; Package Config
(after! company
  (setq company-idle-delay 0.5          ; start auto-complete sooner
        company-minimum-prefix-length 2 ; "
        company-show-numbers t)         ; "
  (add-hook 'evil-mode-state-entry-hook #'company-abort)) ; make aborting less annoying
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(after! js2-mode
  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(remove-hook 'writegood-mode-hook #'writegood-mode)

;; Prettier.js
(add-hook! 
  js2-mode 'prettier-js-mode
  (add-hook 'before-save-hook #'refmt-before-save nil t))

;; Projectile
(setq projectile-project-search-path '("~/code" "~/git"))
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))


;; Calibre
(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Documents/calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))

;; Nov
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 80
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval
                   (doom-modeline-segment--workspace-name))
                  (:eval
                   (doom-modeline-segment--window-number))
                  (:eval
                   (doom-modeline-segment--nov-info))
                  ,(propertize
                    " %P "
                    'face 'doom-modeline-buffer-minor-mode)
                  ,(propertize
                    " "
                    'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                    'display `((space
                                :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(* (let ((width (doom-modeline--font-width)))
                                         (or (and (= width 1) 1)
                                             (/ width (frame-char-width) 1.0)))
                                       (string-width
                                        (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))



;; Ricing Org Mode
;; source: https://lepisma.xyz/2017/10/28/ricing-org-mode/
;; (defvar dark-theme 'doom-gruvbox)
;; (defvar light-theme 'doom-gruvbox-light)

;; (defvar current-theme light-theme
;;   "Currently active color scheme")

;; (defmacro set-pair-faces (themes consts faces-alist)
;;   "Macro for pair setting of custom faces.
;; THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
;;   ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
;; like:
;;   ((face1 theme-one-attr theme-two-atrr)
;;    (face2 theme-one-attr nil           )
;;    (face3 nil            theme-two-attr)
;;    ...)"
;;   (defmacro get-proper-faces ()
;;     `(let* (,@consts)
;;        (backquote ,faces-alist)))

;;   `(setq theming-modifications
;;          ',(mapcar (lambda (theme)
;;                      `(,theme ,@(cl-remove-if
;;                                  (lambda (x) (equal x "NA"))
;;                                  (mapcar (lambda (face)
;;                                            (let ((face-name (car face))
;;                                                  (face-attrs (nth (cl-position theme themes) (cdr face))))
;;                                              (if face-attrs
;;                                                  `(,face-name ,@face-attrs)
;;                                                "NA"))) (get-proper-faces)))))
;;                    themes)))

;; (set-pair-faces
;;  ;; Themes to cycle in
;;  (doom-gruvbox doom-gruvbox-light)

;;  ;; Variables
;;  ((bg-white           "#fbf8ef")
;;   (bg-light           "#222425")
;;   (bg-dark            "#1c1e1f")
;;   (bg-darker          "#1c1c1c")
;;   (fg-white           "#ffffff")
;;   (shade-white        "#efeae9")
;;   (fg-light           "#655370")
;;   (dark-cyan          "#008b8b")
;;   (region-dark        "#2d2e2e")
;;   (region             "#39393d")
;;   (slate              "#8FA1B3")
;;   (keyword            "#f92672")
;;   (comment            "#525254")
;;   (builtin            "#fd971f")
;;   (purple             "#9c91e4")
;;   (doc                "#727280")
;;   (type               "#66d9ef")
;;   (string             "#b6e63e")
;;   (gray-dark          "#999")
;;   (gray               "#bbb")
;;   (sans-font          "Source Sans 3")
;;   (serif-font         "Merriweather")
;;   (et-font            "EtBembo")
;;   (sans-mono-font     "Souce Code Pro")
;;   (serif-mono-font    "Verily Serif Mono")) ;; TODO install this font

;;  ;; Settings
;;  ((variable-pitch
;;    (:family ,sans-font)
;;    (:family ,et-font
;;             :background nil
;;             :foreground ,bg-dark
;;             :height 1.7))
;;   (org-document-title
;;    (:inherit variable-pitch
;;              :height 1.3
;;              :weight normal
;;              :foreground ,gray)
;;    (:inherit nil
;;              :family ,et-font
;;              :height 1.8
;;              :foreground ,bg-dark
;;              :underline nil))
;;   (org-document-info
;;    (:foreground ,gray
;;                 :slant italic)
;;    (:height 1.2
;;             :slant italic))
;;   (org-level-1
;;    (:inherit variable-pitch
;;              :height 1.3
;;              :weight bold
;;              :foreground ,keyword
;;              :background ,bg-dark)
;;    (:inherit nil
;;              :family ,et-font
;;              :height 1.6
;;              :weight normal
;;              :slant normal
;;              :foreground ,bg-dark))
;;   (org-level-2
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.2
;;              :foreground ,gray
;;              :background ,bg-dark)
;;    (:inherit nil
;;              :family ,et-font
;;              :weight normal
;;              :height 1.3
;;              :slant italic
;;              :foreground ,bg-dark))
;;   (org-level-3
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    (:inherit nil
;;              :family ,et-font
;;              :weight normal
;;              :slant italic
;;              :height 1.2
;;              :foreground ,bg-dark))
;;   (org-level-4
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    (:inherit nil
;;              :family ,et-font
;;              :weight normal
;;              :slant italic
;;              :height 1.1
;;              :foreground ,bg-dark))
;;   (org-level-5
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    nil)
;;   (org-level-6
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    nil)
;;   (org-level-7
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    nil)
;;   (org-level-8
;;    (:inherit variable-pitch
;;              :weight bold
;;              :height 1.1
;;              :foreground ,slate
;;              :background ,bg-dark)
;;    nil)
;;   (org-headline-done
;;    (:strike-through t)
;;    (:family ,et-font
;;             :strike-through t))
;;   (org-quote
;;    (:background ,bg-dark)
;;    nil)
;;   (org-block
;;    (:background ,bg-dark)
;;    (:background nil
;;                 :foreground ,bg-dark))
;;   (org-block-begin-line
;;    (:background ,bg-dark)
;;    (:background nil
;;                 :height 0.8
;;                 :family ,sans-mono-font
;;                 :foreground ,slate))
;;   (org-block-end-line
;;    (:background ,bg-dark)
;;    (:background nil
;;                 :height 0.8
;;                 :family ,sans-mono-font
;;                 :foreground ,slate))
;;   (org-document-info-keyword
;;    (:foreground ,comment)
;;    (:height 0.8
;;             :foreground ,gray))
;;   (org-link
;;    (:underline nil
;;                :weight normal
;;                :foreground ,slate)
;;    (:foreground ,bg-dark))
;;   (org-special-keyword
;;    (:height 0.9
;;             :foreground ,comment)
;;    (:family ,sans-mono-font
;;             :height 0.8))
;;   (org-todo
;;    (:foreground ,builtin
;;                 :background ,bg-dark)
;;    nil)
;;   (org-done
;;    (:inherit variable-pitch
;;              :foreground ,dark-cyan
;;              :background ,bg-dark)
;;    nil)
;;   (org-agenda-current-time
;;    (:foreground ,slate)
;;    nil)
;;   (org-hide
;;    nil
;;    (:foreground ,bg-white))
;;   (org-indent
;;    (:inherit org-hide)
;;    (:inherit (org-hide fixed-pitch)))
;;   (org-time-grid
;;    (:foreground ,comment)
;;    nil)
;;   (org-warning
;;    (:foreground ,builtin)
;;    nil)
;;   (org-date
;;    nil
;;    (:family ,sans-mono-font
;;             :height 0.8))
;;   (org-agenda-structure
;;    (:height 1.3
;;             :foreground ,doc
;;             :weight normal
;;             :inherit variable-pitch)
;;    nil)
;;   (org-agenda-date
;;    (:foreground ,doc
;;                 :inherit variable-pitch)
;;    (:inherit variable-pitch
;;              :height 1.1))
;;   (org-agenda-date-today
;;    (:height 1.5
;;             :foreground ,keyword
;;             :inherit variable-pitch)
;;    nil)
;;   (org-agenda-date-weekend
;;    (:inherit org-agenda-date)
;;    nil)
;;   (org-scheduled
;;    (:foreground ,gray)
;;    nil)
;;   (org-upcoming-deadline
;;    (:foreground ,keyword)
;;    nil)
;;   (org-scheduled-today
;;    (:foreground ,fg-white)
;;    nil)
;;   (org-scheduled-previously
;;    (:foreground ,slate)
;;    nil)
;;   (org-agenda-done
;;    (:inherit nil
;;              :strike-through t
;;              :foreground ,doc)
;;    (:strike-through t
;;                     :foreground ,doc))
;;   (org-ellipsis
;;    (:underline nil
;;                :foreground ,comment)
;;    (:underline nil
;;                :foreground ,comment))
;;   (org-tag
;;    (:foreground ,doc)
;;    (:foreground ,doc))
;;   (org-table
;;    (:background nil)
;;    (:family ,serif-mono-font
;;             :height 0.9
;;             :background ,bg-white))
;;   (org-code
;;    (:inherit font-lock-builtin-face)
;;    (:inherit nil
;;              :family ,serif-mono-font
;;              :foreground ,comment
;;              :height 0.9))))


;;; SOURCES
;; This is a list of the sources of inspiration for many of these configurations:
;; - Diego Zamboni's Literate Doom Config: https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
;;   - good ideas on literate config files and working with babel
;;   - TODO look into his Org mode gtd workflow
;; -
