(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq use-package-enable-imenu-support t
      use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics init-file-debug)

(straight-use-package 'use-package)

;; Save customizations to different file
(setq custom-file (expand-file-name "customizations.el" user-emacs-directory))
(load custom-file 'noerror)

(setq vc-follow-symlinks t
      load-prefer-newer t)

;; temporarily disable the file name handler.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; increase garbage collection threshold for initialization
(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))
  (use-package org)
  ;; Load actual configuration.
  (org-babel-load-file "~/.dotfiles/home/emacs-config.org"))

(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.2)

(setq file-name-handler-alist (append default-file-name-handler-alist file-name-handler-alist))
(require 'cl-seq)
(cl-delete-duplicates file-name-handler-alist :test 'equal)
(put 'narrow-to-region 'disabled nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))
