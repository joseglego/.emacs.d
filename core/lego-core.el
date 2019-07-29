;; Basic: User
(setq user-full-name "Jose Lezama")
(setq user-mail-address "me@joseglego.io")

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
 backup-directory-alist '(("." . "~/.emacs_backups"))    ; don't litter my fs tree
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

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(provide 'lego-core)
