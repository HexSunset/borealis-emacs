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

;; Theme
(load-theme 'modus-vivendi)

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
(use-package compile
  :bind ("C-x c" . compile))
(use-package ido
  :config
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
(use-package org)
(use-package org-roam)


;; ---------------
;; -- FUNCTIONS --
;; ---------------
(defun sudo-find-file ()
  "Open the file with sudo through TRAMP"
  (interactive)
    (ido-find-file-in-dir "/sudo::/"))


;; --------------
;; -- SETTINGS --
;; --------------

(put 'dired-find-alternate-file 'disabled nil)

;; backup files are now only in /tmp
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))


;; --------------
;; -- KEYBINDS --
;; --------------

;; sudo-find-file
(global-set-key (kbd "C-x C-r") 'sudo-find-file)

;; window and buffer manipulations
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; other-window
(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "M-o") 'other-window)


;; ------------
;; -- CUSTOM --
;; ------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package org-roam eglot company rust-mode smex magit which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
