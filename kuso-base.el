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

(if (eq (file-exists-p (concat default-directory "../snippets")) nil)
    (make-directory (concat default-directory "../snippets"))
  )

(setq yas-snippet-dirs (concat default-directory "../snippets"))
(cons "../snippets" yas-snippet-dirs)
;(yas/load-directory "../snippets")
(yas-global-mode 1)

;; highlight line mode
(hl-line-toggle-when-idle)

;; ac-dabbrev configuratio
(require 'ac-dabbrev)
(setq ac-sources
     (list ac-source-dabbrev))
(global-set-key (kbd "<backtab>") 'dabbrev-expand)


;; IDO configurations
(ido-mode t)

;; Workgroups configurations
(workgroups-mode t)


;; HideShow
(load-library "hideshow")

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

(global-set-key (kbd "C-\-") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

;; sr-speedbar configuration
(global-set-key (kbd "\C-c ]") 'sr-speedbar-toggle)

;; Setup flymake
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; flymake shortkeys
(global-set-key (kbd "\C-x a") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "\C-x p") 'flymake-goto-next-error)

;; Basic Key bindings
(global-set-key (kbd "\C-c m") 'menu-bar-mode)


;; theme configuration
;; TODO: create a default theme so user can easily change it
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tomorrow-night) ;;-eighties)
))

;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

;; multiple cursor configurations
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-SPC ") 'mc/mark-all-like-this)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)


;; Enhancements ---------------------------------------------
;; Global configurations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq x-select-enable-clipboard t)
(column-number-mode t)
(global-linum-mode)
(menu-bar-mode -1)
(show-paren-mode t)
(autopair-global-mode t)
(cua-selection-mode t)

(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Backup files
;; Put them in one nice place if possible
(if (file-directory-p "~/.backup")
    (setq backup-directory-alist '(("." . "~/.backup")))
  (make-directory "~/.backup"))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)


;; ----------------------------------------------------------

;; Load about submenu
(load-file (concat default-directory "kuso-version.el"))
(load-file (concat default-directory "kuso-about.el"))
(load-file (concat default-directory "kuso-dpaste.el"))
(load-file (concat default-directory "session-management.el"))


(provide 'kuso-base)
