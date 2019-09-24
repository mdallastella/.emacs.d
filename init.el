;;; init.el --- Emacs configuration of Marco Dalla Stella -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Marco Dalla Stella

;; Author: Marco Dalla Stella <marco@dallastella.name>
;; URL: https://github.com/mdallastella/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Code:

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; List package archives and initialize them
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")
	("org"      . "http://orgmode.org/elpa/")))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

(setq load-prefer-newer t)               ; Always load newer compiled files
(setq ad-redefinition-action 'accept)    ; Silence advice redefinition warnings
(setq message-log-max 10000)             ; Debugging

;; Bootstrap use-package and dash
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash)
	     (package-installed-p 'no-littering))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash)
  (package-install 'no-littering))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; I need org-mode
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; Keep .emacs.d clean
(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (setq create-lockfiles nil
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)

  (setq backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/")))
	auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package gnu-elpa-keyring-update
  :ensure t)

;; Load emacs.org - my Emacs configuration
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))

;; My custom file
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(byte-recompile-file custom-file nil 0 t)
(message "Loaded %s" custom-file)

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;;; init.el ends here
