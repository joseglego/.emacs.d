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

(provide 'setup-powerline)
