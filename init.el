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

(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 150)
(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab" :height 150)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 150)
(set-face-attribute 'default nil :family "Iosevka" :height 150)


;; --------------
;; -- PACKAGES --
;; --------------

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'execute-extended-command)

;;(require 'gruber-darker-theme)
(load-theme 'modus-vivendi t)

(require 'ido)
(setq borealis-ido-allow-buffers '("*info*" "*compilation*" "*ansi-term*"))
(setq ido-ignore-buffers '("\\` " borealis-ido-ignore-buffers))

(ido-everywhere 1)

(require 'which-key)
(which-key-mode 1)


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

(global-set-key (kbd "C-c o") 'clone-line-below)

(global-unset-key (kbd "C-M-p"))
(global-set-key (kbd "C-M-p") 'move-text-up)

(global-unset-key (kbd "C-M-n"))
(global-set-key (kbd "C-M-n") 'move-text-down)

(global-set-key (kbd "C-x C-r") 'sudo-find-file)

(global-set-key (kbd "<C-return>") 'open-line-below)

(global-set-key (kbd "<C-M-return>") 'open-line-above)

(global-set-key (kbd "<f5>") 'compile)


;; ------------
;; -- CUSTOM --
;; ------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode gruber-darker-theme company rust-mode smex magit which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
