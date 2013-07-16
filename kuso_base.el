;; Autocomplete configurations
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.kuso.d/ac-dict")
(ac-config-default)

(global-auto-complete-mode t)

;; Tramp configuration
(setq tramp-default-method "ssh")

;; Yasnippet configurations
(add-to-list 'load-path (concat default-directory "../yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)


;; theme configuration
;; TODO: create a default theme so user can easily change it
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tomorrow-night-eighties)
))

;; ac-dabbrev configuratio
(require 'ac-dabbrev)
(setq ac-sources
     (list ac-source-dabbrev))
(global-set-key (kbd "<backtab>") 'dabbrev-expand)


;; Global configurations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq x-select-enable-clipboard t)
(column-number-mode t)
(global-linum-mode)
(menu-bar-mode -1)
(show-paren-mode t)

(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; IDO configurations
(ido-mode t)

;; Workgroups configurations
(workgroups-mode t)


;; HideShow
(load-library "hideshow")
(global-set-key (kbd "C-\-") 'hs-toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
