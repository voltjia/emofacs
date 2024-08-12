;;; pyim
(use-package pyim
  :straight t
  :config
  (setq default-input-method "pyim"))

;;; pyim-basedict
(use-package pyim-basedict
  :straight t
  :config
  (pyim-basedict-enable))

;; Set Chinese font.
(defun get-han-font ()
  (pcase system-type
    ('windows-nt
     (or (car (seq-filter (lambda (font) (member font (font-family-list)))
                          '("Microsoft YaHei" "Microsoft JhengHei" "SimHei")))
         "default-windows-font"))
    ('darwin
     (or (car (seq-filter (lambda (font) (member font (font-family-list)))
                          '("Hei" "Heiti SC" "Heiti TC")))
         "default-mac-font"))
    ('gnu/linux
     (or (car (seq-filter (lambda (font) (member font (font-family-list)))
                          '("WenQuanYi Micro Hei")))
         "default-linux-font"))))

(unless (string-equal (face-attribute 'default :family) "JetBrains Mono")
  (set-fontset-font "fontset-default" 'han (get-han-font)))
