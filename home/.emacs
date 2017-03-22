;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("SC"    . "http://joseito.republika.pl/sunrise-commander/") t)
(setq load-prefer-newer t)
(package-initialize)

;; (setq debug-on-error t)

;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package configures the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Save customizations to different file
(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)

;; Load actual configuration.
(setq vc-follow-symlinks t)
(org-babel-load-file "~/.emacs-config.org")
