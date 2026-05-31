;;; profiles.el -*- lexical-binding: t; -*-
;;
;; Doom's native profile system, replacing Chemacs2 (see ADR-0002).
;;
;; The Doom framework lives at the STANDARD location ~/.emacs.d (a symlink to the
;; doomemacs clone). Running plain `emacs` loads the "global" profile = that
;; install, with this directory (~/.dotfiles/doom, via ~/.config/doom) as its
;; private config. That is the stable daily driver.
;;
;; Extra profiles below are OPT-IN: launch with `emacs --profile NAME` (or
;; DOOMPROFILE=NAME). They are sandboxed -- their packages live under
;; $XDG_DATA_HOME/doom/NAME/ -- so experiments cannot destabilise the daily
;; config. Run `doom sync` after editing this file to regenerate the loader.
;;
;; A profile may point at an entirely non-Doom config via `user-emacs-directory`.
;;
;; Format: (("name" (KEY . VALUE) ...) ...)

(()
 ;; Templates migrated from the old Chemacs emacs-profiles.el. Uncomment and
 ;; `doom sync` to use. See issue 09.
 ;;
 ;; ("scratch"   (user-emacs-directory . "~/.dotfiles/emacs-from-scratch"))
 ;; ("kickstart" (user-emacs-directory . "~/code/kickstart.emacs"))
 ;; ("nano"      (user-emacs-directory . "~/code/nano-emacs"))
 ;; ("prelude"   (user-emacs-directory . "~/code/prelude"))
 )
