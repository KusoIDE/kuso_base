;;; kuso-base.el --- Basic configuration of Kuso IDE

;; Copyright (C) 2012-2014 Sameer Rahmani <lxsameer@gnu.org>

;; Author: Sameer Rahmani <lxsameer@gnu.org>
;; Keywords: lisp kuso IDE base
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains the needed configuration for Kuso IDE very basic
;; functionality

;;; Code:

;; code goes here
(message "Initializing 'kuso-base' plugin.")

;; Shell Confs -------------------------------------------------------

;; Setup PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)


;; Initial configuration ---------------------------------------------

;; Remove splash screen
(setq inhibit-splash-screen t)

;; scratch should be scratch
(setq initial-scratch-message nil)


;; Tramp configuration ---------------------------------------------
(setq tramp-default-method "ssh")

;; Configuring bs -------------------------------------------------
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Smex -----------------------------------------------------------
;(require 'smex)
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)
;; This is the old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; IDO configurations ---------------------------------------------
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode 1)
(setq ido-use-faces nil)
; If don't want to use the flx's highlights you can turn them off like this
; (setq flx-ido-use-faces nil)

(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
;; Workgroups configurations --------------------------------------
;(workgroups-mode t)


;; HideShow -------------------------------------------------------
(global-set-key (kbd "C-\-") 'hs-toggle-hiding)
(hs-minor-mode)


;; replace strings
(global-set-key (kbd "C-c M-s") 'replace-string)

;; flymake shortkeys
;; FIXME: these belongs to programing plugin
;(global-set-key (kbd "\C-x a") 'flycheck-next-error)
;(global-set-key (kbd "\C-x C-a") 'flycheck-previous-error)

;; Basic Key bindings
(global-set-key (kbd "\C-c m") 'menu-bar-mode)


;; Indentation ----------------------------------------------
;; Don't allow tab as indent
(setq-default indent-tabs-mode nil)
;; Default indent width
(setq tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; theme configuration --------------------------------------
;; TODO: create a default theme so user can easily change it
;(eval-after-load "color-theme"
;  '(progn
;     (color-theme-initialize)
;     (color-theme-monokai)
;     ))

;; Multiple cursor -----------------------------------------
;; multiple cursor configurations
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-SPC ") 'mc/mark-all-like-this)

;; expand-region -------------------------------------------
(global-set-key (kbd "C-=") 'er/expand-region)


;; Enhancements ---------------------------------------------
;; Global configurations
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq x-select-enable-clipboard t)
(column-number-mode t)

;; linum mode
(global-linum-mode)
(setq linum-format " %3d ")

(menu-bar-mode -1)
(show-paren-mode t)
(cua-selection-mode t)

;; Backup files ---------------------------------------------
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
(load-file (concat default-directory "custom.el"))

;; Inflections utility
(load-file (concat default-directory "inflections.el"))

;; Key Chord ------------------------------------------------
(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "hj"     'undo)
(key-chord-define-global "kl"     'right-word)
(key-chord-define-global "sd"     'left-word)
(key-chord-define-global "m,"     'forward-paragraph)
(key-chord-define-global "p["     'backward-paragraph)


;; Power Line -----------------------------------------------
(require 'powerline)

;(setq powerline-arrow-shape 'half)   ;; the default
;(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(provide 'kuso-base)
;;; kuso-base.el ends here
