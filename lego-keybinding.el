;; GoTo
(global-set-key [f5] 'goto-line)

;; browse-url-of-file
(global-set-key [f6] 'browse-url-of-file)

;; Magit
(require 'magit-gitflow)
(global-set-key [f7] 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

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

;; Lorem Ipsum
(global-set-key (kbd "C-c l s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c l p") 'lorem-ipsum-insert-paragraphs)
(global-set-key (kbd "C-c l l") 'lorem-ipsum-insert-list)

;; Multiple-Cursors
(require 'multiple-cursors)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'lego-keybinding)
