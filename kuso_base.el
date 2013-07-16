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

;; Autocomplete configurations
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat default-directory "../ac-dict"))
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

;; Load abou submenu
(load-file (concat default-directory "kuso_version.el"))
(load-file (concat default-directory "kuso_about.el"))
(load-file (concat default-directory "kuso_dpaste.el"))
