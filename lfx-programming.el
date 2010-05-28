;; Set ident for C/C++
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

;; Make settings
'(compile-command "make")

;; CEDET
(add-to-list 'load-path "~/emacs/cedet/common")
(require 'cedet)
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(setq which-func-mode t)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
;;(defconst cedet-win32-include-dirs
;;  (list "C:/MinGW/include"
;;        "C:/MinGW/include/c++/3.4.5"
;;        "C:/MinGW/include/c++/3.4.5/mingw32"
;;        "C:/MinGW/include/c++/3.4.5/backward"
;;        "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
;;        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
(require 'semantic-c nil 'noerror)
(let ((include-dirs cedet-user-include-dirs))
;;  (when (eq system-type 'windows-nt)
;;    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))
;;(semantic-load-enable-guady-code-helpers)
;;(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)
;;(setq semantic-decoration-mode nil)

;; CEDET hotkeys
(global-unset-key [C-j])
(global-set-key [f12] 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c s d") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-j") 'semantic-ia-fast-jump)

;; Folding
(when (and window-system (require 'semantic-tag-folding nil 'noerror))
  (global-semantic-tag-folding-mode 1)
  (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
  (define-key semantic-tag-folding-mode-map (kbd "C--") 'semantic-tag-folding-fold-block)
  (define-key semantic-tag-folding-mode-map (kbd "C-=") 'semantic-tag-folding-show-block)
;;  (define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-all)
;;  (define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-show-all)
)

(define-key c-mode-base-map (kbd "M-/") 'semantic-ia-complete-symbol)