;;; solaire-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solaire-mode" "solaire-mode.el" (0 0 0 0))
;;; Generated autoloads from solaire-mode.el

(autoload 'solaire-mode "solaire-mode" "\
Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants.

If called interactively, enable Solaire mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'solaire-global-mode 'globalized-minor-mode t)

(defvar solaire-global-mode nil "\
Non-nil if Solaire-Global mode is enabled.
See the `solaire-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `solaire-global-mode'.")

(custom-autoload 'solaire-global-mode "solaire-mode" nil)

(autoload 'solaire-global-mode "solaire-mode" "\
Toggle Solaire mode in all buffers.
With prefix ARG, enable Solaire-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Solaire mode is enabled in all buffers where
`turn-on-solaire-mode' would do it.
See `solaire-mode' for more information on Solaire mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-solaire-mode "solaire-mode" "\
Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'." t nil)

(autoload 'turn-off-solaire-mode "solaire-mode" "\
Disable `solaire-mode' in the current buffer." t nil)

(autoload 'solaire-mode-in-minibuffer "solaire-mode" "\
Highlight the minibuffer whenever it is active." nil nil)

(autoload 'solaire-mode-reset "solaire-mode" "\
Reset all buffers with `solaire-mode' enabled.

The purpose for this is to reset faces that cannot be buffer-local such as the
fringe, which can be changed by loading a new theme or opening an Emacs client
frame with a different display (via emacsclient).

\(fn &rest _)" t nil)

(autoload 'solaire-mode-reset-buffer "solaire-mode" "\
Reset `solaire-mode' incurrent buffer.
See `solaire-mode-reset' for details." nil nil)

(advice-add #'load-theme :before (lambda (theme &optional _ no-enable) (unless no-enable (setq solaire-mode--current-theme theme))))

(advice-add #'load-theme :after (lambda (theme &rest _) (when (memq theme custom-enabled-themes) (setq solaire-mode--bg-swapped nil) (when (featurep 'solaire-mode) (solaire-mode--swap-bg-faces-maybe)))))

(autoload 'solaire-mode-swap-bg "solaire-mode" "\
Does nothing. Set `solaire-mode-auto-swap-bg' instead." nil nil)

(make-obsolete 'solaire-mode-swap-bg 'solaire-mode-auto-swap-bg '"1.1.4")

(autoload 'solaire-mode-restore-persp-mode-buffers "solaire-mode" "\
Restore `solaire-mode' in buffers when `persp-mode' loads a session.

\(fn &rest _)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solaire-mode" '("solaire-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solaire-mode-autoloads.el ends here
