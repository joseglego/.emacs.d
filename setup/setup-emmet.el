;; WebMode: Emmet-Hook
(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'rjsx-mode-hook 'emmet-mode)

(setq emmet-expand-jsx-className? t) ;; default nil
(setq emmet-preview-default t)

(provide 'setup-emmet)
