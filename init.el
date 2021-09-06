;; Copyright (c) 2020-2021 Jiacheng Huang

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Package Setup
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

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
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(package-selected-packages
   '(powerline zoom treemacs-all-the-icons org-dashboard dashboard lsp-python-ms lsp-java dap-mode which-key magit go-mode exec-path-from-shell rustic rust-mode yasnippet-snippets yasnippet treemacs-projectile treemacs projectile lsp-mode counsel ivy flycheck company smartparens dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Bootstrap John Wigley's "use-package"
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ensure environment variables inside Emacs look the same as in the user's shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Dracula
(load-theme 'dracula t)

;; All The Icons
(require 'all-the-icons)
(when (display-graphic-p)
  (unless (member "all-the-icons" (font-family-list)) (all-the-icons-install-fonts)))

;; Power Line
(require 'powerline)
(powerline-default-theme)

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "E + mofa + cs")
(setq dashboard-startup-banner 'logo)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

;; Zoom
(zoom-mode)
(setq zoom-size '(0.618 . 0.618))

;; Ivy
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Projectile
(projectile-mode 1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Treemacs
(require 'treemacs)
(treemacs-follow-mode t)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode t)
(setq treemacs-show-cursor t)
(setq treemacs-width 32)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(define-key global-map (kbd "M-0") 'treemacs-select-window)

;; Treemacs All The Icons
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

;; Treemacs Projectile
(require 'treemacs-projectile)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Which Key
(which-key-mode)

;; YASnippet
(yas-global-mode 1)

(defvar my-company-point nil)
(advice-add 'company-complete-common
            :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common
            :after (lambda ()
  		     (when (equal my-company-point (point))
  		       (yas-expand))))

;; LSP
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'go-mode-hook 'lsp)
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'js-mode-hook 'lsp)
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook #'lsp)
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))
(add-hook 'rust-mode-hook 'lsp)
(setq lsp-clients-clangd-args
      '("--header-insertion=never"
        "--header-insertion-decorators=0"))
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; Flycheck
(add-hook 'c-mode-hook #'flycheck-mode)
(add-hook 'c++-mode-hook #'flycheck-mode)
(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'java-mode-hook #'flycheck-mode)
(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'python-mode-hook #'flycheck-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)

;; Smartparens
(require 'smartparens-config)
(add-hook 'asm-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'java-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'rust-mode-hook #'smartparens-mode)
(add-hook 'rustic-mode-hook #'smartparens-mode)
(add-hook 'verilog-mode #'smartparens-mode)

;; When you press RET, the curly braces automatically add another newline.
(sp-with-modes '(asm-mode c-mode c++-mode emacs-lisp-mode java-mode js-mode python-mode rust-mode rustic-mode verilog-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC") ("* ||\n[i]" "RET"))))
(setq sp-escape-quotes-after-insert nil)

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

;; Initialize frame size and position
(defun initialize-frame ()
  (let* ((base-factor 0.5)
	 (display-width (* (display-pixel-width) base-factor))
         (display-height (* (display-pixel-height) base-factor))
         (display-left (truncate (/ (- (display-pixel-width) display-width) 2)))
	 (display-top (truncate (/ (- (display-pixel-height) display-height) 2))))
    (set-frame-position (selected-frame) display-left display-top)
    (set-frame-size (selected-frame) (truncate display-width) (truncate display-height) t)))
(setq frame-resize-pixelwise t)
(when (display-graphic-p) (initialize-frame))

;; Place auto-save files into system's temporary file
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Tabs
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

;; Delete Selection Mode
(delete-selection-mode 1)

;; Org Mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; CC Mode
(require 'cc-mode)
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(c-set-offset 'innamespace 0)

;; Asm Mode
(defun my-asm-mode-hook ()
  (local-unset-key (vector asm-comment-char))
  (setq tab-always-indent (default-value 'tab-always-indent)))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; Rust Mode
(require 'rust-mode)
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;; Rustic Mode
(use-package rustic)

;; JavaScript Mode
(setq js-indent-level 2)
