;; Copyright (c) 2020 Jiacheng Huang

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets yasnippet treemacs-projectile treemacs-all-the-icons all-the-icons treemacs projectile lsp-mode counsel ivy flycheck company smartparens dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Dracula
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Ivy
(counsel-mode 1)

;; Projectile
(projectile-mode 1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Treemacs
(require 'treemacs)
(setq treemacs-width 24)
(treemacs-follow-mode t)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode t)
(setq treemacs-show-cursor t)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(define-key global-map (kbd "M-0") 'treemacs-select-window)

(treemacs-load-all-the-icons-with-workaround-font "Hermit")

(require 'treemacs-projectile)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

;; LSP
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq lsp-clients-clangd-args
      '("--header-insertion=never"
        "--header-insertion-decorators=0"))

;; YASnippet
(yas-global-mode 1)

(defvar my-company-point nil)
(advice-add 'company-complete-common
            :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common
            :after (lambda ()
  		     (when (equal my-company-point (point))
  		       (yas-expand))))

;; Flycheck
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-standard-library "libc++")))
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

;; Window Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

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

;; Org Mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; CC Mode
(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(c-set-offset 'innamespace 0)

;; Asm Mode
(setq electric-indent-inhibit t)

;; Verilog Mode
(setq verilog-auto-newline nil)
