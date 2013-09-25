;;   Kuso IDE
;;    Copyright (C) 2010-2013  Sameer Rahmani <lxsameer@gnu.org>
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
(message "Initializing 'kuso-base' plugin.")

;; Autocomplete configurations
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat default-directory "../ac-dict"))
(ac-config-default)

;; Remove return key from completion map
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)

(global-auto-complete-mode t)

;; Tramp configuration
(setq tramp-default-method "ssh")

;; Yasnippet configurations
(add-to-list 'load-path (concat default-directory "../yasnippet"))
(yas-global-mode 1)


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
(autopair-global-mode t)

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

;; sr-speedbar configuration
(global-set-key (kbd "\C-c ]") 'sr-speedbar-toggle)

;; Setup flymake
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; flymake shortkeys
(global-set-key (kbd "\C-x a") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "\C-x p") 'flymake-goto-next-error)


;; theme configuration
;; TODO: create a default theme so user can easily change it
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tomorrow-night) ;;-eighties)
))


;; Load about submenu
(load-file (concat default-directory "kuso-version.el"))
(load-file (concat default-directory "kuso-about.el"))
(load-file (concat default-directory "kuso-dpaste.el"))


(provide 'kuso-base)
