;; TypeScript

(require 'flycheck)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;;
(setq typescript-indent-offset 2)
(setq typescript-indent-level 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq typescript-indent-offset 2)
(setq typescript-indent-level 2)

;; TSX
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'typescript-mode-hook (lambda ()
                           (company-mode)
                           (smartparens-mode)
                           (prettier-js-mode)
                           ))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'typescript-mode)

(provide 'setup-ts-mode)
