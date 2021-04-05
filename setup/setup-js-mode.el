;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; .eslintrc
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;; indent section
(setq js-indent-level 2)
(setq js-highlight-level 3)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)
(setq js-highlight-level 3)

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
(require 'company-tabnine)

(add-hook 'js2-mode-hook (lambda ()
                           (company-mode)
                           (smartparens-mode)))

(add-hook 'rjsx-mode-hook (lambda ()
                           (company-mode)
                           (smartparens-mode)))

(add-hook 'typescript-mode-hook (lambda ()
                           (company-mode)
                           (smartparens-mode)))

(provide 'setup-js-mode)
