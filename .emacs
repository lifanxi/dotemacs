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

;; User Information
(setq user-full-name "Byron Li")
(setq user-mail-address "byron_li@trendmicro.com.cn")

;; Text
(load "lfx-text")

;; Programming 
(load "lfx-programming")

;; Muse
(load "lfx-muse")

;; Hotkeys
(load "lfx-hotkeys")

