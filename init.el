;; Basic: User
(setq user-full-name "Jose Lezama")
(setq user-mail-address "me@joseglego.io")

;; Basic: Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Basic: Theme
(package-initialize)
(require 'color-theme)
(load-theme 'darkokai t)

;; Basic: Show line number
(global-linum-mode 1)
(line-number-mode 1) 

;; Basic: Show column number
(column-number-mode 1) 

;; Basic: NO scrollbar, NO toolbar NO menubar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Basic: Load mouse wheel
(mwheel-install)  

;; Basic: Doesn't show the nouse on the buffers 
(mouse-avoidance-mode "animate")

;; Basic: Delete the selected element if paste while it's selected
(delete-selection-mode 1) 

;; Basic: Move Between buffers using shift+arrow
(windmove-default-keybindings)

;; Basic: Electric pair mode
(electric-pair-mode 1)
(push '(?\' . ?\') electric-pair-pairs)
(push '(?\` . ?\`) electric-pair-pairs)

;; Basic: Hightlight parenthesis that closes/opens the selected parenthesis
(show-paren-mode 1) 

;; Basic: Set F5 as Go-To  Line 
(global-set-key [f5] 'goto-line)

;; F6 = browse-url-of-file
(global-set-key [f6] 'browse-url-of-file)

;; F7: Magit
(require 'magit-gitflow)
(global-set-key [f7] 'magit-status)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Basic: Turn off bugging yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Basic: WhiteSpace Cleanup
(global-whitespace-cleanup-mode)

;; Basic: Use spaces instead spaces
(setq-default indent-tabs-mode nil)

;; Basic: Maximize Buffer
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; C-c <left> to go back

;; Saves emacs backups in other folder
(setq
 backup-by-copying t              ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs_backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)               ; use versioned backups

;; Basic: Auto revert mode (Updating buffers when changed on disk)
(global-auto-revert-mode)

;; Move Backup files.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Execute shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Web Mode: Begin
(require 'web-mode)
(require 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[ea]rb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[pdj]html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'" . ruby-mode))

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

(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook #'emmet-mode)

(setq emmet-preview-default t)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors scss-mode vue-mode yaml-mode js-doc magit-gitflow web-mode typescript-mode rbenv powerline multi-web-mode json-mode js2-mode helm flycheck exec-path-from-shell darkokai-theme color-theme))))

;;;; AutoComplete Family
;; Helm Mode
(require 'helm)
(require 'helm-config)

(with-eval-after-load "helm"
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'web-mode)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; AngularJS: AutoComplete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/angular-snippets/snippets/")
(add-to-list 'ac-modes 'angular-mode)
(add-to-list 'ac-modes 'angular-html-mode)

;; Lorem Ipsum
(global-set-key (kbd "C-c l s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c l p") 'lorem-ipsum-insert-paragraphs)
(global-set-key (kbd "C-c l l") 'lorem-ipsum-insert-list)

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
(rbenv-use "2.4.2")


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

;; Associates json-mode to all .eslintrc files
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

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

(add-to-list 'flycheck-checkers 'javascript-eslint)
(add-to-list 'flycheck-checkers 'scss-stylelint)
(add-to-list 'flycheck-checkers 'ruby-rubocop)
(add-to-list 'flycheck-checkers 'typescript-tslint)
(add-to-list 'flycheck-checkers 'python-flake8)
(add-to-list 'flycheck-checkers 'html-tidy)

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
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
(global-set-key (kbd "C-x g") 'magit-status)


;; Multiple-Cursors
(require 'multiple-cursors)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
