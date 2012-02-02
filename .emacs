;; General Settings

(add-to-list 'load-path
             "~/emacs")

(setq current-language-environment "UTF-8")
(setq visible-bell)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq kill-ring-max 100)
(setq default-fill-column 75)
(setq default-major-mode 'text-mode)
(setq show-paren-mode t)
(setq show-paren-style 'parentheses)
(setq frame-title-format "emacs@%b")

(setq global-font-lock-mode 1)
(setq transient-mark-mode t)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)

(setq mouse-yank-at-point t)
(mouse-avoidance-mode 'animate)

(fset 'yes-or-no-p 'y-or-n-p)

(setq tool-bar-mode nil)
;; (menu-bar-mode nil)

;; Backup Settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
     '(("." . "~/.saves"))  ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; User Information
(setq user-full-name "Li Fanxi")
(setq user-mail-address "lifanxi@freemindworld.com")

;; Save desktop
(require 'desktop)
(desktop-save-mode 1)

;; Unicad
;; Auto detect file encoding
(load "unicad")

;; Text
(load "lfx-text")

;; Programming 
(load "lfx-programming")

;; Muse
;; No longer using muse
;; (load "lfx-muse")

;; Hotkeys
(load "lfx-hotkeys")

;; Efan
(load "lfx-fanfou")