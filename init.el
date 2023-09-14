;; -*- lexical-binding: t; -*-
;;
;; ██████╗  ██████╗ ██████╗ ███████╗ █████╗ ██╗     ██╗███████╗
;; ██╔══██╗██╔═══██╗██╔══██╗██╔════╝██╔══██╗██║     ██║██╔════╝
;; ██████╔╝██║   ██║██████╔╝█████╗  ███████║██║     ██║███████╗
;; ██╔══██╗██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██║╚════██║
;; ██████╔╝╚██████╔╝██║  ██║███████╗██║  ██║███████╗██║███████║
;; ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝╚══════╝
;;
;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;; █████╗  ██╔████╔██║███████║██║     ███████╗
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;; config by Aurora <3

(load-file (concat user-emacs-directory "borealis-fn.el"))


;; ----------------
;; -- APPEARANCE --
;; ----------------

;; Disable unnecessary UI
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(add-to-list 'default-frame-alist
	     '(font . "Iosevka-15"))


;; --------------
;; -- PACKAGES --
;; --------------

(require 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package smex
  :bind ("M-x" . smex))
(use-package gruber-darker-theme
  :init (load-theme 'gruber-darker t))
(use-package compile
  :init (setq compile-command nil)
  :bind ("C-x c" . compile))
(use-package ido
  :config
  (setq ido-ignore-buffers '("\\` " "\\`*[:alnum:]*\\`*"))
  (ido-everywhere))
(use-package company
  :config
  (setq company-idle-delay nil)
  (global-company-mode)
  :bind ("C-<tab>" . company-complete))
(use-package eglot)
(use-package dired-x)
(use-package magit)
(use-package which-key
  :config
  (which-key-mode))
(use-package calendar
  :config
  (setq calendar-week-start-day 1))
(use-package typst-mode)
(use-package org
  :init
  (setq org-directory (make-directory-if-not "~/org"))
  :config
  (setq org-agenda-files (directory-files org-directory))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))
(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-directory (make-directory-if-not "~/roam"))
  (org-roam-setup))
(use-package slime
  :init (setq inferior-lisp-program "sbcl"))
(use-package rust-mode)


;; --------------
;; -- SETTINGS --
;; --------------
;; settings that don't belong with a specific package

;; I'm tired of having to write out yes and no
(setq use-short-answers t)

(put 'dired-find-alternate-file 'disabled nil)

(global-auto-revert-mode 1)
;; auto refresh things like dired, quietly
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; backup files are now only in /tmp
(setq backup-directory-alist
      `((".*" . ,(make-directory-if-not (concat user-emacs-directory "backups/")))))
(setq auto-save-file-name-transforms
      `((".*" ,(make-directory-if-not (concat user-emacs-directory "auto-save/")) t)))


;; --------------
;; -- KEYBINDS --
;; --------------
;; binds that don't fit under a specific package

;; window and buffer manipulations
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x 9") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "C-x o")) ;other-window
(global-set-key (kbd "C-x o") 'clone-line-below)

(global-unset-key (kbd "C-M-p"))
(global-set-key (kbd "C-M-p") 'move-text-up)

(global-unset-key (kbd "C-M-n"))
(global-set-key (kbd "C-M-n") 'move-text-down)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-x C-r") 'sudo-find-file)

(global-set-key (kbd "C-x C-o") 'find-org-file)

(global-set-key (kbd "<C-return>") 'open-line-below)

(global-set-key (kbd "<C-M-return>") 'open-line-above)

(global-unset-key (kbd "C-x C-n")) ;set-goal-column
(global-set-key (kbd "C-x C-n") 'revert-buffer-quick)

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-unset-key (kbd "M-h")) ;mark-paragraph
(global-set-key (kbd "M-h") 'backward-kill-word)


;; ------------
;; -- CUSTOM --
;; ------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(smartparens rainbow-delimiters slime paredit markdown-preview-mode markdown-mode gruber-darker-theme use-package org-roam eglot company rust-mode smex magit which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
