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

;;;; AutoComplete Family

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(require 'react-snippets)
(yas-global-mode 1)

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
(require 'setup-markdown-mode)

(require 'setup-flycheck)
(require 'setup-show-hide)
(require 'setup-powerline)
(require 'setup-emmet)
