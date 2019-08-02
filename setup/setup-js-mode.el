;; JS & TypeScript
(add-to-list 'auto-mode-alist '("\\.js[x]\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js2-mode))

;; .eslintrc
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;; indent section
(setq js-indent-level 2)
(setq js-highlight-level 3)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)
(setq js-highlight-level 3)
(setq typescript-indent-offset 2)
(setq typescript-indent-level 2)

(setq json-reformat:indent-width 2)

(add-hook 'js-mode-hook
          #'(lambda ()
              (define-key js-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js-mode-map "@" 'js-doc-insert-tag)))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "js")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(require 'company)
(require 'company-tern)

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(add-hook 'rjsx-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(add-hook 'js2-mode-hook 'flow-minor-mode)
(add-hook 'rjsx-mode-hook 'flow-minor-mode)

(provide 'setup-js-mode)
