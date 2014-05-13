;;   Kuso IDE
;;    Copyright (C) 2010-2014  Sameer Rahmani <lxsameer@gnu.org>
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
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
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
(global-set-key (kbd "\C-x a") 'flycheck-next-error)
(global-set-key (kbd "\C-x C-a") 'flycheck-previous-error)

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
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-monokai)
     ))

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


;; Power Line -----------------------------------------------
(require 'powerline)

;(setq powerline-arrow-shape 'half)   ;; the default
;(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

;; Do What I Mean --------------------------------------------
(defun back-to-indentation-or-beginning ()
  "This function switches the point to before the first non-space character, or if the point is already there it goes to the beginning of the line. "
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun end-of-code-or-line+ (arg)
  "Move to the end of code. If already there, move to the end of line,
  that is after the possible comment. If at the end of line, move to the
  end of code.
  Comments are recognized in any mode that sets syntax-ppss properly."
  (interactive "P")
  (let ((eoc (save-excursion
               (move-end-of-line arg)
               (while (point-in-comment)
                 (backward-char))
               (skip-chars-backward " \t")
               (point))))
    (cond ((= (point) eoc)
           (move-end-of-line arg))
          (t
           (move-end-of-line arg)
           (while (point-in-comment)
             (backward-char))
           (skip-chars-backward " \t")))))

;; Thus, ‘M-w’ with no selection copies the current line, ‘C-w’ kills it entirely, and ‘C-a M-w C-y’ duplicates it.
;; http://www.emacswiki.org/emacs/?action=browse;oldid=SlickCopy;id=WholeLineOrRegion
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))


;; http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(global-set-key (kbd "M-;") 'comment-dwim-line)
(global-set-key (kbd "C-x C-e") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "M-S-<down>") 'duplicate-start-of-line-or-region)

(provide 'kuso-base)
