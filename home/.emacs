;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("SC"    . "http://joseito.republika.pl/sunrise-commander/") t)
(setq load-prefer-newer t)
(package-initialize)

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

;; Load actual configuration.
(setq vc-follow-symlinks t)
(org-babel-load-file "~/.emacs-config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("322539e9fe165e118cbc427db7ce934a1fc80a956df1dff00f599f519040737e" default)))
 '(package-selected-packages
   (quote
    (openwith uncrustify-mode uncrustify writegood-mode write-good write-good-mode org-dashboard dummyparens legalese fix-word logview logview-mode demangle-mode ws-butler charmap bar-cursor aggressive-fill-paragraph syslog-mode smart-dash ace-link origami helm-eshell urlenc term-run xterm-color window-numbering which-key wgrep-helm wgrep-ag volatile-highlights use-package undo-tree unbound tramp-term switch-window sunrise-commander sudo-edit string-inflection spinner speed-type smartwin smartscan smartparens smart-tabs-mode smart-shift smart-forward simple-call-tree shift-text shelldoc shell-switcher shell-history shell-here shell-command rtags rebox2 realgud rainbow-mode rainbow-identifiers rainbow-delimiters pretty-mode preproc-font-lock powerline popwin persistent-scratch pdf-tools page-break-lines package-safe-delete org-linkany org-dotemacs org-bullets org-autolist org-alert on-screen num3-mode nlinum never-comment neotree multishell multifiles multi-eshell modern-cpp-font-lock minimap minibuffer-line magit-rockstar magit-filenotify linum-off js2-mode itail indent-guide iedit hungry-delete hlinum hl-todo hl-sexp hl-indent highlight-indent-guides highlight-blocks helm-projectile helm-package helm-mt helm-make helm-ls-git helm-ispell helm-git helm-fuzzier helm-flyspell helm-flycheck helm-flx helm-dash helm-company helm-c-yasnippet helm-c-moccur helm-anything helm-ag goto-last-change git-timemachine git-messenger git-gutter+ german-holidays flymake-cppcheck flycheck-pos-tip flycheck-clangcheck fill-column-indicator eshell-fringe-status eshell-did-you-mean emojify embrace elf-mode electric-spacing edit-at-point duplicate-thing direx diredful dired-imenu dired-efap diffview diff-hl dictcc describe-number cursor-chg context-coloring company-shell company-quickhelp company-qml company-c-headers comment-dwim-2 color-identifiers-mode cmake-ide cmake-font-lock clang-format centered-cursor-mode bury-successful-compilation beacon bash-completion babel avy-menu autopair autobookmarks auto-package-update auto-indent-mode auto-highlight-symbol auto-dim-other-buffers auto-compile auto-capitalize atom-dark-theme annotate all-the-icons all-ext adaptive-wrap ace-isearch))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(demangled ((t nil)))
 '(mangled ((t nil)))
 '(writegood-duplicates-face ((t (:inherit flyspell-duplicate))))
 '(writegood-passive-voice-face ((t (:inherit flyspell-duplicate))))
 '(writegood-weasels-face ((t (:inherit flyspell-incorrect)))))
