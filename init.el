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

;; Basic: Hightlight parenthesis that closes/opens the selected parenthesis
(show-paren-mode 1) 

;; Basic: Set F5 as Go-To  Line 
(global-set-key [f5] 'goto-line)

;; Basic: Turn off bugging yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Basic: WhiteSpace Cleanup
(global-whitespace-cleanup-mode)

;; Basic: Use spaces instead spaces
(setq-default indent-tabs-mode nil)

;;;; AutoComplete Family
;; Helm Mode
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Web Mode: Begin
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

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
(setq js-indent-level 2)
(setq js-highlight-level 3)

(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook #'emmet-mode)
(setq emmet-preview-default t)
(add-hook 'web-mode-hook 'rainbow-mode)

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;;;; AutoComplete Family
;; Helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

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

(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc -c ~/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone"))
