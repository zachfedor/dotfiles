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
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :style "Retina" :size 14 :weight 'semi-light)
      ;; doom-variable-pitch-font (font-spec :family "Cormorant Garamond" :style "Regular" :size 14 :weight 'medium)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :style "Retina" :size 24 :weight 'semi-light))


(setq
 ;; No worries with auto-saved buffers
 auto-save-default t
 ;; Enable granular insert-mode history for undoing
 evil-want-fine-undo t
 ;; Set initial size on start
 initial-frame-alist '((top . 20) (left . 20) (width . 154) (height . 49))
 ;; Use Unicode ellipses instead of three periods for space efficiency
 truncate-string-ellipsis "…"
 ;; Raise undo history to 80Mb
 undo-limit 80000000
 ;; Enable visual line-based editing
 visual-line-mode 1
 ;; Stretch cursor to contain entire glyph
 x-stretch-cursor t)

(setq-default
 history-length 1000)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light
      doom-font-increment 1
      line-spacing 0.3)
(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(after! doom-modeline
  (setq display-time-format "⏱ %H:%M"
        display-time-default-load-average nil)
  (display-time-mode))

;; Theme specific customizations
(setq default-text-properties '(line-spacing 0.25 line-height 1.25))
(cond ((equal doom-theme 'doom-gruvbox)
       (custom-set-faces!
         `(font-lock-keyword-face :slant italic :foreground ,(doom-color 'default))
         `(org-drawer :size 14 :foreground ,(doom-color 'grey))
         `(outline-1 :foreground ,(doom-color 'grey) :weight extra-bold :height 1.60)
         `(outline-2 :foreground ,(doom-color 'grey) :weight extra-bold :height 1.40)
         `(outline-3 :foreground ,(doom-color 'default) :weight bold :height 1.00)
         `(outline-4 :foreground ,(doom-color 'default) :weight bold :height 1.00)
         `(outline-5 :foreground ,(doom-color 'default) :weight bold :height 1.00)))
      ;; Global theme customizations if no themes match
      (t
       (custom-set-faces!
         '(font-lock-comment-face :slant italic)
         `(font-lock-keyword-face :slant italic))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/notes")
;; (defun zf/org-mode-setup ()
;;   (setq display-line-numbers nil)
;;   ;; (org-indent-mode)
;;   )
(use-package! org
  ;; :hook (org-mode . zf/org-mode-setup)
  :config
  (setq-default major-mode 'org-mode)
  (setq org-deadline-warning-days 7
        org-ellipsis " ▾ "
        org-hide-emphasis-markers t
        org-list-demote-modify-bullet '(("1" . "a") ("a" . "1"))
        org-log-done 'time
        org-log-into-drawer t
        org-roam-directory "~/Dropbox/notes/roam"
        org-tags-column -80))

;; (setq-hook! org-mode
;;   org-fontify-done-headline t
;;   org-fontify-quote-and-verse-blocks t
;;   org-fontify-whole-heading-line t
;;   org-image-actual-width '(800))

;; (defun add-created-timestamp-property ()
;;   "Add CREATED property to current item with timestamp"
;;   (interactive)
;;   (org-set-property "CREATED" (format-time-string "%F")))
;; (add-hook 'org-capture-before-finalize-hook 'add-created-timestamp-property)

;; TODO: figure out a way to limit this to org-mode, maybe on `SPC m e'
;; (map!
;;  :desc "Eval org src block and print results" "C-c C-c" #'org-babel-execute-src-block)

;; Zero-width spaces! A hacky way to get syntax formatting to work for adjacent blocks
(map! :map org-mode-map
      :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))

;;; Org-Superstar
(use-package! org-superstar
  :config (setq org-superstar-cycle-headline-bullets nil  ;; don't cycle, just repeat the last bullet in list
                org-superstar-headline-bullets-list '(?◉ ?○ ?▷)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! deft
  :config
  (setq deft-directory org-directory  ;; deft should use same notes directory as org-mode
        deft-default-extension "org"  ;; create new files with this filetype
        deft-extensions '("org" "md") ;; include these filetype in searches
        deft-org-mode-title-prefix t  ;; generated titles in new files are prefixed with `#+title'
        deft-recursive t  ;; search sub-directories too
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t  ;; use the search string text to create new files using `deft-file-naming-rules'
        deft-file-naming-rules '((nospace . "-") (case-fn . downcase))))  ;; convert spaces to hyphens and downcase all text
(map!
 :leader
 :desc "Open deft" "DEL" #'deft
 :prefix "f"
 :desc "Deft find file" "n" #'deft-find-file)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (defun zf/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package! visual-fill-column
;;   :hook (org-mode . zf/org-mode-visual-fill))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
;; (use-package! mixed-pitch
;;   :hook (org-mode . mixed-pitch-mode)
;;   :config
;;   (setq mixed-pitch-set-height t)
;;   (set-face-attribute 'variable-pitch nil :height 1.3))

;; Keybindings
(use-package! which-key
  :config
  (setq which-key-allow-multiple-replacements t
        which-key-idle-delay 0.4
        which-key-use-C-h-commands t)
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "ě-\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "ȅ-\\1"))
   ))
;; (map!
;;  :leader
;;  :desc "M-x" "SPC" #'execute-extended-command
;;  :desc "Open vterm" "`" #'+vterm/here)

;; Visuals
(setq scroll-margin 6)
(map!
 :desc "Scroll down" "C-j" #'evil-scroll-down
 :desc "Scroll up" "C-k" #'evil-scroll-up)

(defun zf/toggle-transparency ()
  "Toggle frame transparency between 100% to 85%"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (equal (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive> form
                      ((numberp (cadr alpha)) (cadr alpha)))
                100)
         '(94 . 70) '(100 . 100)))))
(map!
 :leader
 :prefix "t"
 :desc "Transparency" "t" #'zf/toggle-transparency
 :desc "Zone out" "o" #'zone)

(map!
 :leader
 :prefix "s"
 :desc "Hide highlights" "h" #'evil-ex-nohighlight)


;; Force new windows to open to the right and below the current window
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; After opening a new window, prompt to switch buffer
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+vertico/switch-workspace-buffer))

(use-package! writegood-mode
  :config
  (map! :leader
        :prefix "t"
        :desc "Grammar checker" "G" #'writegood-mode)
  (map! :localleader
        :map writegood-mode-map
        "g" #'writegood-grade-level
        "r" #'writegood-reading-ease))


(defun z/downcase-org-keywords ()
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

(defun z/downcase-org-properties ()
  "Convert Org mode property drawers to lowercase in current buffer.

TODO: there's definitely a better way to accomplish this...

Example: :PROPERTIES: -> :properties:

Inspired by above z/downcase-org-keywords function"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward "\\(?1:\\:\\(?:PROPERTIES\\|LOGBOOK\\|END\\)\\:\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil ))
      (message "Downcased %d matches" count))))

(defun z/cider-debug-toggle-insert-state ()
  (if cider--debug-mode    ;; Checks if you're entering the debugger
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state

(add-hook 'cider--debug-mode-hook 'z/cider-debug-toggle-insert-state)

(setq typescript-indent-level 2)
