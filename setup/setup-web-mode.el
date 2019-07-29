;; Define Major mode for Web Dev
;; HTML/Pug & CSS
(require 'web-mode)

;; Files
(add-to-list 'auto-mode-alist '("\\.[pdj]html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . pug-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css\\'" . web-mode))

;; Not my Common languages
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))

;; WebMode: Style-Gook
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
)

;; indent section
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq scss-indent-offset 2)

;; rainbow mode for colors
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Multi-Web-Mode
(setq mweb-default-major-mode 'web-mode)
(setq mweb-tags '((web-mode "<template>" "</template>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "ctp" "phtml" "vue"))
(multi-web-global-mode 1)

(provide 'setup-web-mode)
