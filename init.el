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

;;; Ivy
(use-package ivy
  :straight t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

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

;;; which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;;; ace-window
(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;;; Company
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t)
  :bind
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)))

;;; Iedit
(use-package iedit
  :straight t)

;;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; all-the-icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts)))

;;; Emojify
(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode))

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
  :straight t
  :after (treemacs projectile))

;;; treemacs-icons-dired
(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;;; treemacs-magit
(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;;; treemacs-all-the-icons
(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

;;; Smartparens
(use-package smartparens-config
  :straight smartparens
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;;; Zenburn
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;;; Dashboard
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "E + mofa + cs"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((projects . 8)
                          (recents . 8))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-set-init-info t))

;;; powerline
(use-package powerline
  :straight t
  :config
  (powerline-default-theme))

;;; Term Mode
(use-package term
  :config
  (define-key term-raw-map (kbd "C-x") nil)
  (define-key term-raw-map (kbd "M-o") nil)
  (define-key term-raw-map (kbd "M-0") nil))

;; Place auto-save files to a dedicated directory.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Replace highlighted text with what I type.
(delete-selection-mode 1)

;; Keybindings to resize windows.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Turn off alarms completely.
(setq ring-bell-function 'ignore)

;; Display line numbers.
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Display the current column number.
(setq column-number-mode t)

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the tool bar.
(tool-bar-mode -1)

;; Start Emacs in fullscreen mode.
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

;; Only immediately display :emergency and :error warnings.
(setq warning-minimum-level :error)

;; Use spaces instead of tabs when indenting.
(setq-default indent-tabs-mode nil)

;; Avoid saving active regions to the primary selection.
(setq select-active-regions nil)

;; Set the font to JetBrains Mono if it's available.
(if (member "JetBrains Mono" (font-family-list))
    (set-face-attribute 'default nil :family "JetBrains Mono"))
