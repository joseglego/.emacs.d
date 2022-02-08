;;; setup-flycheck.el --- Setup for linters
;;
;;; Commentary:
;;
;;; Code:

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Define Flycheckers - Linters
(add-to-list 'flycheck-checkers 'html-tidy)
(add-to-list 'flycheck-checkers 'scss-stylelint)
(add-to-list 'flycheck-checkers 'javascript-eslint)
(add-to-list 'flycheck-checkers 'typescript-tslint)

(add-to-list 'flycheck-checkers 'ruby-rubocop)
(add-to-list 'flycheck-checkers 'python-flake8)

;; Define Flycheckers - Modes
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'scss-stylelint 'scss-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'typescript-tslint 'typescript-mode)

(flycheck-add-mode 'ruby-rubocop 'ruby-mode)
(flycheck-add-mode 'python-flake8 'python-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-eslint-rules-directories '(""))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefixs ".flycheck")
(setq-default flycheck-stylelintrc ".stylelintrc")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

;; Use correct version of rubocop
(setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(eval-after-load 'rjsx
  '(add-hook 'rjsx-hook #'add-node-modules-path))

(provide 'setup-flycheck)
