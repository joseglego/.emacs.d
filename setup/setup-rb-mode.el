;; Ruby & RoR

;; Files
(add-to-list 'auto-mode-alist '("\\.[ea]rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode))

;; Use rbenv and ruby 2.4.4 for rubocop
(require 'rbenv)
(global-rbenv-mode)
(rbenv-use "2.4.4")

;; ruby-mode NO coding: UTF8
(setq ruby-insert-encoding-magic-comment nil)

(provide 'setup-rb-mode)
