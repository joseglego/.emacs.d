;;; init.el --- josegLEGO Emacs configuration
;;
;;; Commentary:
;;
;; It's my basic configuration for Emacs as text editor.
;; It's mainly focus on Web Development with JS, Vue/React, RoR/Django/Node
;;
;;; Code:

(package-initialize)

(setq lego-packages (expand-file-name "lego-packages.el" user-emacs-directory))
(load lego-packages)

(setq lego-ui (expand-file-name "lego-ui.el" user-emacs-directory))
(load lego-ui)

(setq lego-keybinding (expand-file-name "lego-keybinding.el" user-emacs-directory))
(load lego-keybinding)

(setq lego-core (expand-file-name "lego-core.el" user-emacs-directory))
(load lego-core)

;; Define Major mode for Web Dev
(require 'web-mode)
;; HTML/SCSS
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[pdj]html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . pug-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css\\'" . web-mode))

;; JS
(add-to-list 'auto-mode-alist '("\\.js[x]\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js2-mode))

;; .eslintrc
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.[ea]rb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode))

;; Not Common
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

;; WebMode: Emmet-Hook
(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; Indent
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq js-indent-level 2)
(setq js-highlight-level 3)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)
(setq js-highlight-level 3)
(setq typescript-indent-offset 2)
(setq typescript-indent-level 2)
(setq scss-indent-offset 2)
(setq json-reformat:indent-width 2)

(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'rjsx-mode-hook 'emmet-mode)

(setq emmet-expand-jsx-className? t) ;; default nil
(setq emmet-preview-default t)

(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Powerline
(require 'powerline)
(powerline-default-theme)
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

;; Activate Show&Hide Minor Mode!
(defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

(defun toggle-hiding (column)
       (interactive "P")
       (if hs-minor-mode
         (if (condition-case nil
                              (hs-toggle-hiding)
                            (error t))
                          (hs-show-all))
                    (toggle-selective-display column)))
(load-library "hideshow")
  (global-set-key (kbd "C-+") 'toggle-hiding)
  (global-set-key (kbd "C-\\") 'toggle-selective-display)

  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'lisp-mode-hook       'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (add-hook 'js-mode-hook         'hs-minor-mode)
  (add-hook 'web-mode-hook        'hs-minor-mode)

;;
(require 'rbenv)
(global-rbenv-mode)
(rbenv-use "2.4.4")


;; Basic: JSDoc Mode
(require 'js-doc)
(setq js-doc-mail-address "me@joseglego.io"
       js-doc-author (format "Jose Lezama <%s>" js-doc-mail-address)
       js-doc-url "joseglego.io"
       js-doc-license "MIT")

 (add-hook 'js-mode-hook
           #'(lambda ()
               (define-key js-mode-map "\C-ci" 'js-doc-insert-function-doc)
               (define-key js-mode-map "@" 'js-doc-insert-tag)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "js")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(put 'downcase-region 'disabled nil)

;; Flycheck Family
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(add-to-list 'flycheck-checkers 'javascript-eslint)
(add-to-list 'flycheck-checkers 'scss-stylelint)
(add-to-list 'flycheck-checkers 'ruby-rubocop)
(add-to-list 'flycheck-checkers 'typescript-tslint)
(add-to-list 'flycheck-checkers 'python-flake8)
(add-to-list 'flycheck-checkers 'html-tidy)

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'scss-stylelint 'scss-mode)
(flycheck-add-mode 'ruby-rubocop 'ruby-mode)
(flycheck-add-mode 'python-flake8 'python-mode)
(flycheck-add-mode 'typescript-tslint 'typescript-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-eslint-rules-directories '(""))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefixs ".flycheck")
(setq-default flycheck-stylelintrc ".stylelintrc")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

;; Magit
(setq magit-save-repository-buffers nil)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")

;; ruby-mode NO coding: UTF8
(setq ruby-insert-encoding-magic-comment nil)

;; Multi-Web-Mode
(setq mweb-default-major-mode 'web-mode)
(setq mweb-tags '((web-mode "<template>" "</template>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "ctp" "phtml" "vue"))
(multi-web-global-mode 1)
