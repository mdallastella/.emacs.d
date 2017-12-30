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
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")
	("org"      . "http://orgmode.org/elpa/")))
(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

;; Bootstrap use-package and dash
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash)
	     (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash)
  (package-install 'diminish))

;; Make sure Org is installed
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

;; Show a stacktrace whene an error occurs
(setq stack-trace-on-error t)

;; Load emacs.org - my Emacs configuration
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))

;; My custom file
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(byte-recompile-file custom-file nil 0 t)
(message "Loaded %s" custom-file)

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
;;; init.el ends here
