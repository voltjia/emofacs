;;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use-package
(straight-use-package 'use-package)

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :config
  (exec-path-from-shell-initialize))

;;; Zenburn
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;;; all-the-icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts)))

;;; powerline
(use-package powerline
  :straight t
  :config
  (powerline-default-theme))

;;; Dashboard
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "E + mofa + cs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 8)
                          (recents . 8)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t))

;;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; Iedit
(use-package iedit
  :straight t)

;;; Ivy
(use-package ivy
  :straight t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;;; Swiper
(use-package swiper
  :straight t
  :after (ivy)
  :config
  (global-set-key "\C-s" 'swiper))

;;; Counsel
(use-package counsel
  :straight t
  :after (swiper)
  :config
  (counsel-mode))

;;; Treemacs
(use-package treemacs
  :straight t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-show-cursor t
        treemacs-width 32)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)))

;;; treemacs-projectile
(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

;;; treemacs-icons-dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

;;; treemacs-magit
(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

;;; treemacs-all-the-icons
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :straight t
  :config
  (treemacs-load-theme "all-the-icons"))

;;; Company
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t))

;;; which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;;; LSP
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (go-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         (java-mode . lsp)
         (js-mode . lsp)
         (python-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;;; lsp-ui
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;;; lsp-ivy
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

;;; lsp-treemacs
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;;; Flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;;; Smartparens
(use-package smartparens-config
  :straight smartparens
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;;; ace-window
(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; Turn off alarms completely.
(setq ring-bell-function 'ignore)

;; Keybindings to resize windows.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Display line numbers.
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Display the current column number.
(setq column-number-mode t)

;; Start Emacs in fullscreen mode.
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

;; Place auto-save files to a dedicated directory.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Replace highlighted text with what I type.
(delete-selection-mode 1)

;; Use spaces instead of tabs when indenting.
(setq-default indent-tabs-mode nil)
