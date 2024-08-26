(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (if (executable-find "pandoc")
      (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
    (message "Pandoc is not found. Markdown preview may not work as expected.")))
