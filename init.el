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
	("org"      . "https://orgmode.org/elpa/")
        ("MELPA"    . "https://melpa.org/packages/")))
(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

;; Bootstrap use-package and dash
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash)
	     (package-installed-p 'diminish)
	     (package-installed-p 'no-littering))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash)
  (package-install 'diminish)
  (package-install 'no-littering))

;; Make sure Org is installed
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

;; Show a stacktrace whene an error occurs
(setq stack-trace-on-error t)

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

;; Load emacs.org - my Emacs configuration
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))

;; My custom file
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(byte-recompile-file custom-file nil 0 t)
(message "Loaded %s" custom-file)

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;;; init.el ends here
