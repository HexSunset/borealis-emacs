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


(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'borealis-fn)

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

;; Fix builtin defaults of cc-mode derivatives
(add-hook 'c-mode-common-hook #'be/fix-cc-mode-indentation)


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

(use-package multiple-cursors
  :bind (("C-c n" . 'mc/mark-next-like-this)
	 ("C-c p" . 'mc/mark-previous-like-this)
	 ("C-c a" . 'mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-." . 'er/expand-region)
	 ("C-," . 'er/contract-region)))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package eglot)

(use-package company
  :config
  (setq company-idle-delay nil)
  (global-company-mode)
  :bind ("C-<tab>" . company-complete))

(use-package rust-mode
  :mode "\\.rs\\'")


;; --------------
;; -- KEYBINDS --
;; --------------
;; binds that don't fit under a specific package

;; make M-f act more like "w" in vim
(require 'misc)
(keymap-global-set "M-f" 'forward-to-word)
(keymap-global-set "M-F" 'forward-word)
(keymap-global-set "M-B" 'backward-to-word)

;; window and buffer manipulations
(keymap-global-set "C-x C-k" 'kill-current-buffer)
(keymap-global-set "C-x 9" 'kill-buffer-and-window)
(keymap-global-set "C-x C-b" 'ibuffer)

(keymap-global-set "C-c o" 'be/clone-line-below)

(keymap-global-unset "C-M-p")
(keymap-global-set "C-M-p" 'be/move-text-up)

(keymap-global-unset "C-M-n")
(keymap-global-set "C-M-n" 'be/move-text-down)

(keymap-global-set "C-x C-r" 'be/sudo-find-file)

(keymap-global-set "C-<return>" 'be/open-line-below)
(keymap-global-set "C-M-<return>" 'be/open-line-above)

(keymap-global-set "C-c c" 'compile)


;; ------------
;; -- CUSTOM --
;; ------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
