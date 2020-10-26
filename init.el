(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(counsel ivy flycheck company smartparens dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ivy
(counsel-mode 1)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++20")))
(add-hook 'c-mode-hook 'flycheck-mode)

;; Smartparens
(require 'smartparens-config)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'asm-mode-hook #'smartparens-mode)
;; When you press RET, the curly braces automatically add another newline.
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))
(setq sp-escape-quotes-after-insert nil)

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Winner
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Alarm Bell
(setq ring-bell-function 'ignore)

;; Face Attributes
(set-face-foreground 'mode-line-inactive "light gray")
(set-face-foreground 'mode-line "white")
(set-face-foreground 'line-number "gray")
(set-face-foreground 'font-lock-comment-face "gray")
(set-face-foreground 'font-lock-comment-delimiter-face "gray")
(set-face-foreground 'font-lock-doc-face "gray")

;; Line Number Mode
(if (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; Column Number Mode
(column-number-mode 1)

;; Tabs
(setq-default indent-tabs-mode nil)

;; CC Mode
(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)

;; Asm Mode
(setq-default electric-indent-inhibit t)
