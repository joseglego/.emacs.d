;;; init.el --- josegLEGO Emacs configuration
;;
;;; Commentary:
;;
;; It's my basic configuration for Emacs as text editor.
;; It's mainly focus on Web Development with JS, Vue/React, RoR/Django/Node
;;
;;; Code:

(package-initialize)

;; Define Folders
(defvar core-dir (expand-file-name "core" user-emacs-directory)
  "Directory containing core configuration.")
(defvar setup-dir (expand-file-name "setup" user-emacs-directory)
  "Directory containing modes configuration.")

;; Define packages from Folders
(add-to-list 'load-path core-dir)
(add-to-list 'load-path setup-dir)

;; Require core config
(require 'lego-packages)
(require 'lego-ui)
(require 'lego-core)
(require 'lego-keybinding)

;; Require modes config
(require 'setup-web-mode)
(require 'setup-js-mode)
(require 'setup-rb-mode)
(require 'setup-markdown-mode)

(require 'setup-flycheck)
(require 'setup-show-hide)
(require 'setup-powerline)
(require 'setup-emmet)
