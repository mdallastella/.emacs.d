;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2021 Marco Dalla Stella

;; Author: Marco Dalla Stella <marco@dallastella.name>
;; URL: https://github.com/mdallastella/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Code:

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-splash-screen t
      use-dialog-box nil
      use-file-dialog nil
      inhibit-startup-echo-area-message "mds"
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t)
(fset 'yes-or-no-p 'y-or-n-p)

;;; early-init.el ends here
