;;; base16-dotfiles-theme.el --- base16 theme sourced from the active scripts/theme-switch scheme -*- lexical-binding: t; -*-
;;; Commentary:
;; Colors live in base16-dotfiles-colors.el (loaded below), not here — see
;; that file's commentary for why it's split out (ADR-0009). Loading it again
;; here (rather than assuming config.el already did) is what makes
;; `zf/reload-base16-theme' (SPC t r) actually pick up a fresh
;; `scripts/theme-switch' run: `load-theme' re-executes this file, which
;; re-reads theme-colors.el off disk each time.
;;; Code:
(require 'base16-theme)
(load (expand-file-name "base16-dotfiles-colors.el" (file-name-directory
                                                       (or load-file-name buffer-file-name)))
      nil 'nomessage)

(deftheme base16-dotfiles "Base16 theme driven by the active theme-switch scheme.")
(base16-theme-define 'base16-dotfiles base16-dotfiles-theme-colors)
(provide-theme 'base16-dotfiles)

(provide 'base16-dotfiles-theme)
;;; base16-dotfiles-theme.el ends here
