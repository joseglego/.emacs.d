;;; ui.el --- josegLEGO default ui configuration
;;
;;; Commentary:
;;
;; My default configuration for UI of the editor
;;
;;; Code:

;; Basic: Theme
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

;; Basic: Electric pair mode
(electric-pair-mode 1)
(push '(?\' . ?\') electric-pair-pairs)
(push '(?\` . ?\`) electric-pair-pairs)

;; Basic: Turn off bugging yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'lego-ui)
