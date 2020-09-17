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

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "*.tmp")
     (add-to-list 'grep-find-ignored-directories ".next")
     (add-to-list 'grep-find-ignored-directories "node_modules")))

;; Require core config
(require 'lego-packages)
(require 'lego-ui)
(require 'lego-core)
(require 'lego-keybinding)

;; Require modes config
(require 'setup-web-mode)
(require 'setup-js-mode)
(require 'setup-ts-mode)
(require 'setup-rb-mode)
(require 'setup-markdown-mode)

(require 'setup-flycheck)
(require 'setup-show-hide)
(require 'setup-powerline)
(require 'setup-emmet)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (butler dockerfile-mode docker-compose-mode docker dashboard all-the-icons groovy-mode smartparens flow-minor-mode lorem-ipsum js2-refactor xref-js2 company company-tern manage-minor-mode rjsx-mode pug-mode flycheck markdown-preview-eww markdown-mode emmet-mode react-snippets multiple-cursors scss-mode yaml-mode js-doc magit-gitflow web-mode typescript-mode rbenv powerline multi-web-mode json-mode js2-mode helm exec-path-from-shell darkokai-theme whitespace-cleanup-mode neotree yasnippet auto-complete flycheck-kotlin kotlin-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#6d0204" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#000000" :bold t))))
 '(mode-line-inactive ((t (:foreground "#ffffff" :background "#5d6365" :box nil))))
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#ff6365" :box nil))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#5d6365" :box nil))))
 '(powerline-arrow-shape (quote arrow)))
