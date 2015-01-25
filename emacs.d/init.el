;; -*- mode: elisp -*-

;; load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; load packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(load "better-defaults")

;; load theme
(load-theme 'spacegray t)

;; disable splash screen (re-enable with o)
(setq inhibit-splash-screen t)

;; enable transient mark mode
(transient-mark-mode 1)

;; enable emacsserver
(server-mode 1)

;; enable C-x k to close emacsclient
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;; open new buffers in original frame
(setq ns-pop-up-frames nil)

;; enable C-n movement to add new lines
(setq next-line-add-newlines t)

;; enable copy-paste
(setq x-select-enable-clipboard t)

;;;; ORG MODE
;; enable org-mode
(require 'org)

;; make org-mode work with .org extensions
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; ^ is default in recent emacsen

;; creating org workflow states
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "HOLD" "DONE")))

(setq org-todo-keyword-faces
           '(("TODO" . (:foreground "#268bd2" :weight bold)) ("HOLD" . (:foreground "#268bd2" :weight bold)) ("NEXT" . (:foreground "#2aa198" :weight bold))))

;; david o'toole's org tutorial configuration
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;; EVIL MODE
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; evil mode key bindings

;; -- BEGIN -> set jk to ESC
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                           nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                                              (list evt))))))))
;; from: http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
;; -- END --> set jk to ESC

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;;; WEB MODE
;; enable web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))









