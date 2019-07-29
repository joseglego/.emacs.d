;;; init.el --- josegLEGO Emacs configuration
;;
;;; Commentary:
;;
;; It's my basic configuration for Emacs as text editor.
;; It's mainly focus on Web Development with JS, Vue/React, RoR/Django/Node
;;
;;; Code:

(package-initialize)

(defvar core-dir (expand-file-name "core" user-emacs-directory)
  "Directory containing core configuration.")

(add-to-list 'load-path core-dir)

;; Require core config
(require 'lego-packages)
(require 'lego-ui)
(require 'lego-core)
(require 'lego-keybinding)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-command "pandoc -c ~/.emacs.d/github-pandoc.css -s")

;;;; AutoComplete Family

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(require 'react-snippets)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/snippets")
(ac-config-default)
(add-to-list 'ac-modes 'web-mode)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; AngularJS: AutoComplete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/angular-snippets/snippets/")
(add-to-list 'ac-modes 'angular-mode)
(add-to-list 'ac-modes 'angular-html-mode)

;; Basic: JSDoc Mode
(require 'js-doc)
(setq js-doc-mail-address "me@joseglego.io"
       js-doc-author (format "Jose Lezama <%s>" js-doc-mail-address)
       js-doc-url "joseglego.io"
       js-doc-license "MIT")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Magit
(setq magit-save-repository-buffers nil)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(defvar setup-dir (expand-file-name "setup" user-emacs-directory)
  "Directory containing core configuration.")

(add-to-list 'load-path setup-dir)

;; Require core config
(require 'setup-web-mode)
(require 'setup-js-mode)
(require 'setup-rb-mode)

(require 'setup-flycheck)
(require 'setup-show-hide)
(require 'setup-powerline)
(require 'setup-emmet)
