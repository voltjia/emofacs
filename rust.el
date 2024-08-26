(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))
