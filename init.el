;;; init.el --- Emacs configuration of Marco Dalla Stella -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Marco Dalla Stella

;; Author: Marco Dalla Stella <marco@dallastella.name>
;; URL: https://github.com/mdallastella/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Code:
(setq stack-trace-on-error t)

;;; Tune up garbage collector
(setq gc-cons-threshold most-positive-fixnum)

;; Reset threshold to its default after Emacs has startup, because a large
;; GC threshold equates to longer delays whenever GC happens
(defun my-set-gc-threshold ()
  "Reset gc-cons-threshold to its default value"
  (setq gc-cons-threshold 800000))
(add-hook 'emacs-startup-hook 'my-set-gc-threshold)

;;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))
(package-initialize)

;; Always load newer compiled files
(setq load-prefer-newer t)

;; Silence advice redefinition warnings
(setq ad-redefinition-action 'accept)

;; Debuggingq
(setq message-log-max 10000)

;; Bootstrap `use-package' and `dash'
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash))

(require 'use-package)
(require 'dash)

;;; Validation
(use-package validate                   ; Validate options
  :ensure t)

;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p))
(exec-path-from-shell-initialize)

;;; Load my configuration
(require 'org)
(require 'ob-tangle)

;; tangle and load the init code if needed
(let* ((init-org-file (expand-file-name "main.org" user-emacs-directory))
       (base-name (file-name-sans-extension init-org-file))
       (exported-file (concat base-name ".el")))
  (when (file-newer-than-file-p init-org-file exported-file)
    (org-babel-tangle-file init-org-file exported-file))
  (require 'bytecomp)
  (byte-recompile-file exported-file nil 0 t)
  (message "Loaded %s" exported-file))

(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(byte-recompile-file custom-file nil 0 t)
(message "Loaded %s" custom-file)
