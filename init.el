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

;; Fix emacs having gaps while fullscreen
(setq frame-resize-pixelwise t)

;; Disable unnecessary UI
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 150)
(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab" :height 150)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 150)
(set-face-attribute 'default nil :family "Iosevka" :height 150)


;; --------------
;; -- SETTINGS --
;; --------------
;; settings that don't belong with a specific package

(setq use-short-answers t)

(setq dired-listing-switches "-alh")
(put 'dired-find-alternate-file 'disabled nil)

;; auto refresh things like dired, quietly
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; backup files are now only in /tmp
(setq backup-directory-alist
      `((".*" . ,(be/make-directory-if-not (concat user-emacs-directory "backups/")))))
(setq auto-save-file-name-transforms
      `((".*" ,(be/make-directory-if-not (concat user-emacs-directory "auto-save/")) t)))


;; --------------
;; -- PACKAGES --
;; --------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(be/require 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . execute-extended-command)))

(add-hook 'c-mode-hook #'be/cc-mode-fix-indents)
(add-hook 'c++-mode-hook #'be/cc-mode-fix-indents)

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package ido
  :config
  (setq be/ido-allow-buffers '("*info*" "*compilation*" "*ansi-term*"))
  (setq ido-ignore-buffers '("\\` " be/ido-ignore-buffers))
  (ido-everywhere 1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package magit)

(use-package yasnippet)

(use-package eglot)

(use-package company
  :config
  (setq company-idle-delay nil)
  (global-company-mode)
  :bind ("C-<tab>" . company-complete))

(use-package rust-mode
  :mode "\\.rs\\`")


;; --------------
;; -- KEYBINDS --
;; --------------
;; binds that don't fit under a specific package

;; window and buffer manipulations
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x 9") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c o") 'be/clone-line-below)

(global-unset-key (kbd "C-M-p"))
(global-set-key (kbd "C-M-p") 'be/move-text-up)

(global-unset-key (kbd "C-M-n"))
(global-set-key (kbd "C-M-n") 'be/move-text-down)

(global-set-key (kbd "C-x C-r") 'be/sudo-find-file)

(global-set-key (kbd "<C-return>") 'be/open-line-below)

(global-set-key (kbd "<C-M-return>") 'be/open-line-above)

(global-set-key (kbd "C-c c") 'compile)


;; ------------
;; -- CUSTOM --
;; ------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot yasnippet use-package markdown-mode gruber-darker-theme company rust-mode smex magit which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
