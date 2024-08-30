;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Zach Fedor"
      user-mail-address "zachfedor@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Hack" :size 15.0))
;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :style "Retina" :size 14 :weight 'semi-light)
;;       ;; doom-variable-pitch-font (font-spec :family "ETBembo" :size 24)
;;       doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :style "Retina" :size 24 :weight 'semi-light))

(setq
 ;; No worries with auto-saved buffers
 auto-save-default t
 make-backup-files t
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
 x-stretch-cursor t
 ;; standardize indent and tab widths
 standard-indent 2
 evil-shift-width 2
 tab-width 2)

(setq-default
 history-length 1000)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord
      doom-font-increment 1)
(setq line-spacing 0.3)
(setq-default line-spacing line-spacing)
(setq-default header-line-format " ")

(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(after! doom-modeline
  (setq display-time-day-and-date t
        display-time-24hr-format t
        display-time-load-average nil
        display-time-default-load-average nil)
  (display-time-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Theme specific customizations
(custom-set-faces!
  `(outline-1 :foreground ,(doom-color 'blue))
  `(outline-2 :foreground ,(doom-color 'default))
  `(outline-3 :foreground ,(doom-color 'default))
  `(outline-4 :foreground ,(doom-color 'default))
  `(outline-5 :foreground ,(doom-color 'default))
  `(outline-6 :foreground ,(doom-color 'default))
  `(outline-7 :foreground ,(doom-color 'default))
  `(outline-8 :foreground ,(doom-color 'default))
  `(header-line :background ,(doom-color 'bg)))
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
(setq org-directory (expand-file-name "~/Documents/notes"))
;; (defun zf/org-mode-setup ()
;;   (setq display-line-numbers nil)
;;   ;; (org-indent-mode)
;;   )
(use-package! org
  ;; :hook (org-mode . zf/org-mode-setup)
  :config
  ;; Use Org-Mode as the default major mode for any new buffer
  (setq-default major-mode 'org-mode)
  (setq org-archive-location "::datetree/"
        org-deadline-warning-days 7
        org-default-notes-file (concat org-directory "/inbox.org")
        org-ellipsis " ▾ "
        org-hide-emphasis-markers t
        org-list-demote-modify-bullet '(("1" . "a") ("a" . "1"))
        org-log-done 'time
        org-log-into-drawer t
        org-startup-folded t
        ;; This doom function prevents final state in =org-cycle=, limiting you to seeing heading or just children
        ;; Removing it from their tab hook allows 3rd state of seeing entire subtree under a headine
        org-tab-first-hook (delete '+org-cycle-only-current-subtree-h org-tab-first-hook)
        org-tags-column -80
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d)")
                            (sequence "WAIT(w@/!)" "|" "CLOSED(D!)" "MEETING")
                            (sequence "[ ](c)" "[-](C)" "|" "[X](x)"))
        org-todo-keyword-faces `(("NEXT" . +org-todo-active) ("WAIT" . +org-todo-onhold) ("MEETING" . +org-todo-project) ("[-]" . +org-todo-active))
        org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-capture-templates '(("t" "todo" entry (file org-default-notes-file)
                                 ;; task headline, inactive timestamp, annotation link
                                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                ("n" "note" entry (file org-default-notes-file)
                                 ;; simple headline, annotation link
                                 "* %?\n%a\n")
                                ("m" "meeting" entry (file org-default-notes-file)
                                 ;; meeting headline and tag
                                 "* MEETING with %? :meeting:\n%U\n" :clock-in t :clock-resume t)
                                ("j" "journal" entry (file+datetree +org-capture-journal-file)
                                 ;; simple headline, inactive timestamp
                                 "* %?\n%U\n" :clock-in t :clock-resume t)
                                ("f" "food log" entry (file+datetree (concat org-directory "/food.org"))
                                 ;; simple headline, inactive timestamp, with properties
                                 "* %?\n%U\n"))))

(use-package! denote
  :config
  (setq denote-directory org-directory)
  ;; TODO finish configuring denote
  (setq denote-known-keywords '("fun" "home" "work" "ref"))
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (denote-rename-buffer-mode 1))
(map! :after denote :leader :prefix "n" :desc "Open denote" "d" #'denote)

(use-package! corfu
  :config
  (setq corfu-count 10)
  (setq corfu-max-width 40)
  (setq corfu-min-width corfu-max-width)
  (setq corfu-popupinfo-min-height 5)
  (setq corfu-popupinfo-max-width 80)
  (setq corfu-popupinfo-min-width corfu-popupinfo-max-width)
  (setq corfu-preselect 'valid)
  (setq corfu-scroll-margin 4))

(defun zf/open-inbox ()
  "Open org-mode inbox file"
  (interactive)
  (find-file org-default-notes-file))

;; (map! :after evil :leader :prefix "n" :desc "Open inbox" "n" #'zf/open-inbox)
;; (which-key-add-key-based-replacements "SPC n n" "Open inbox")

;; Save all org buffers 1 minute before the hour, every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(defun zf/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'zf/remove-empty-drawer-on-clock-out 'append)

;; (map! :desc "Complete Word" :map help-map "c" #'+company/complete)

;; (defun zf/toggle-auto-completion ()
;;   "Set `company-idle-delay' to nil for certain modes, preventing auto-complete
;; popup. You can still manually request completion with `C-hc' in insert mode."
;;   (setq-local company-idle-delay nil))

;; (add-hook 'org-mode-hook 'zf/toggle-auto-completion)

;; (use-package! company
;;   :config
;;   (setq company-idle-delay 0.2))

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

;; Use org-mode's open at point function everywhere
(map! :desc "Open At Point" :nie "s-<return>" #'org-open-at-point-global)

;;; Org-Superstar
(use-package! org-superstar
  :config (setq org-superstar-cycle-headline-bullets t  ;; don't cycle, just repeat the last bullet in list
                org-superstar-headline-bullets-list '(?◉ ?○ ?▷)))

;; (use-package! org-appear
;;   :hook (org-mode . org-appear-mode)
;;   :config
;;   (setq org-appear-autoemphasis t
;;         org-appear-autosubmarkers t
;;         org-appear-autolinks nil)
;;   (run-at-time nil nil #'org-appear--set-elements))

;; (use-package! deft
;;   :config
;;   (setq
;;    ;; deft should use same notes directory as org-mode
;;    deft-directory org-directory
;;    ;; create new files with this filetype
;;    deft-default-extension "org"
;;    ;; include these filetype in searches
;;    deft-extensions '("org" "md")
;;    ;; generated titles in new files are prefixed with `#+title'
;;    deft-org-mode-title-prefix t
;;    ;; search sub-directories too
;;    deft-recursive t
;;    deft-use-filename-as-title nil
;;    ;; use the search string text to create new files using `deft-file-naming-rules'
;;    deft-use-filter-string-for-filename t
;;    ;; convert spaces to hyphens and downcase all text
;;    deft-file-naming-rules '((nospace . "-") (case-fn . downcase))
;;    ;; prevent properties or logbook entries from showing in the summary
;;    deft-strip-summary-regexp "\\`\\(.+\n\\)+\n"))

;; (map!
;;  :leader
;;  :desc "Open deft" "DEL" #'deft
;;  :prefix "f"
;;  :desc "Deft find file" "n" #'deft-find-file)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package! projectile
  :config
  ;; Set directories to search within for projects, looking one directory deep
  (setq projectile-project-search-path '(("~/code" . 1) ("~/git" . 1))
        ;; Open magit status buffer after switching to a new project
        ;; TODO: doom uses some other hack which includes opening/switching to workspace, then doom-project-find-file
        ;; projectile-switch-project-action #'magit-status
        +workspaces-switch-project-function #'magit-status
        ;; Use a separate uniquely named buffer for each project, rather than *compilation*
        projectile-per-project-compilation-buffer t
        ;; Use interactive buffers for projectile commands
        projectile-run-use-comint-mode t
        projectile-test-use-comint-mode t
        projectile-compile-use-comint-mode t)
  ;; Add dotfiles as the first project if it isn't already known, which also triggers a scan of new
  ;; projects in search path defined above
  (unless (member "~/.dotfiles" projectile-known-projects)
    (projectile-add-known-project "~/.dotfiles")
    (projectile-discover-projects-in-search-path)))

(map!
 :leader
 :prefix "c"
 :desc "Explain error" "x" #'flycheck-explain-error-at-point)
(map!
 :leader
 :prefix "c"
 :desc "List all errors" "X" #'+default/diagnostics)
(map!
 :leader
 :desc "Sort lines" ">" #'sort-lines
 :desc "Sort fields" "<" #'sort-fields)

;; (defun zf/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package! visual-fill-column
;;   :hook (org-mode . zf/org-mode-visual-fill))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
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
