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

;; Bootstrap use-package, dash and no-littering
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash)
             (package-installed-p 'no-littering))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash)
  (package-install 'no-littering))

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

  (setq create-lockfiles nil
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; My secrets
(let ((secret.el (expand-file-name ".secrets/.secret.el.gpg" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load-library secret.el)))

(let ((authinfo.gpg (expand-file-name ".secrets/.authinfo.gpg" user-emacs-directory)))
  (setq auth-sources `((:source ,authinfo.gpg))))

;; Load emacs.org - my Emacs configuration
(message "Loading emacs.org...")
(let* ((emacs-org-file (expand-file-name "emacs.org" user-emacs-directory))
       (emacs-el-file (concat (file-name-sans-extension emacs-org-file) ".el")))
  (if (file-newer-than-file-p emacs-org-file emacs-el-file)
      (org-babel-load-file emacs-org-file t)
    (load-file emacs-el-file)))

;; My custom file
(message "Loading custom-file...")
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file)

;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024)
                                       read-process-output-max (* 1024 1024))))
;;; init.el ends here

