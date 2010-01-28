(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(case-fold-search t)
 ;; '(current-language-environment "English")
 '(current-language-environment "UTF-8")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 )

;; General and global settings
(add-to-list 'load-path
             "~/emacs")
(setq visible-bell)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq kill-ring-max 100)
(setq default-fill-column 75)
(setq default-major-mode 'text-mode)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(setq frame-title-format "emacs@%b")
(setq mouse-yank-at-point t)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode nil)
(menu-bar-mode nil)
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000)
(mouse-avoidance-mode 'animate)
(custom-set-variables '(line-spacing 5))
(setq-default truncate-partial-width-windows nil)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(auto-image-file-mode)
(partial-completion-mode 1)
(icomplete-mode 1)
(auto-compression-mode 1)
(mouse-wheel-mode t)
(setq x-select-enable-clipboard t)
(setq bookmark-default-file "~/emacs/emacs.bmk")
(global-hl-line-mode 1)
(set-face-background 'hl-line "#fafaff")  ;; Emacs 22 Only
;(set-face-background 'highlight "#330")  ;; Emacs 21 Only
;; (setq lazy-lock-defer-on-scrolling t)
;; (setq font-lock-support-mode 'lazy-lock-mode)
;; (setq font-lock-maximum-decoration t)

;; Backup settings
(setq make-backup-files t)
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-version 2)
(setq dired-kept-version 1)
(setq backup-directory-alist '(("" . "~/emacs/backup")))

;; Dired settings
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; Chinese Related
;; (setq sentence-end "TODO:fill in here ")
;; (setq sentence-end-double-space nil)

;; Date & Time 
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)
(display-time-mode 1)

;; Calendar
(setq calendar-latitude +36.05)
(setq calendar-longitude +118.8)
(setq calendar-location-name "Nanjing")
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)

;; TODO
(setq todo-file-do "~/emacs/todo/do")
(setq todo-file-done "~/emacs/todo/done")
(setq todo-file-top "~/emacs/todo/top")

;; Diary
(setq diary-file "~/emacs/diary")

;; Appointment
(setq add-issue-message t)

;; Hotkey bindings
(global-set-key [f1] 'woman)
(global-set-key [f3] 'ff-find-other-file)
(global-set-key [f4] 'goto-line)
(global-set-key [f5] 'shell)
(global-set-key [f7] 'compile)
'(compile-command "make")
(global-set-key [f8] 'calendar)
(global-set-key [f11] 'bs-cycle-previous)
(global-set-key [f12] 'bs-cycle-next)
;; (global-set-key [(control z)] 'undo)

;; Tab Settings
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)
    ))

(global-set-key [(meta ?/)] 'my-indent-or-complete)

(setq hippie-expand-try-functions-list
      '(
        senator-try-expand-semantic
        try-expand-line
        try-expand-line-all-buffers
        try-expand-list
        try-expand-list-all-buffers
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-file-name-partially
        try-complete-lisp-symbol
        try-complete-lisp-symbol-partially
        try-expand-whole-kill))

;; Programming settings
(add-hook 'c-mode-hook
          '(lambda ( )
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             (setq default-tab-width 4)
             ;;          (hl-line-mode)
             ))
(add-hook 'c++-mode-hook
          '(lambda ( )
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             (setq default-tab-width 4)
             ;;          (hl-line-mode)
             ))

;; File extension to mode
(setq auto-mode-alist
      (append '(("\\.js$" . java-mode)) auto-mode-alist))

(setq user-full-name "Li Fanxi")
(setq user-mail-address "lifanxi@freemindworld.com")


;; Emacs Muse for blogging (Using Muse 3.12)
(add-to-list 'load-path
             "~/emacs/muse")
(load "lfx-muse.el")

;; Xcscope                                                                                                                                                                        
(load-file "~/emacs/cscope-15.7a/contrib/xcscope/xcscope.el")


(server-start)
