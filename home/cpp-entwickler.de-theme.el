(deftheme cpp-entwickler.de
  "Created 2017-09-04.")

(custom-theme-set-faces
 'cpp-entwickler.de
 '(cursor ((t (:background "white smoke"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:font "-outline-Arial-normal-normal-normal-sans-*-*-*-*-p-*-iso8859-1")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "royal blue" :weight bold))))
 '(highlight ((t (:background "gray40" :distant-foreground "black"))))
 '(region ((t (:background "gray20" :distant-foreground "white smoke"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "dark gray"))))
 '(font-lock-builtin-face ((t (:foreground "turquoise1" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "dark gray" :slant italic :height 0.85))))
 '(font-lock-constant-face ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold :inverse-video t))))
 '(font-lock-keyword-face ((t (:foreground "medium orchid" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "DarkOrange1" :weight ultra-bold))))
 '(font-lock-preprocessor-face ((t (:foreground "saddle brown" :slant oblique :weight bold))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((((class grayscale) (background light)) (:slant italic :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8)) (:foreground "green")) (t (:slant italic))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:box (:line-width 2 :color "grey75" :style released-button) :inherit link))))
 '(link ((t (:underline t :weight bold))))
 '(link-visited ((t (:inherit link :weight light))))
 '(fringe ((t (:background "gray5"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "gray10" :foreground "white smoke" :weight normal))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:inherit mode-line :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(powerline-active1 ((t (:background "gray40" :inherit mode-line))))
 '(powerline-active2 ((t (:background "royal blue" :inherit mode-line))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray20"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "gray30"))))
 '(helm-source-header ((t (:inherit mode-line :foreground "white smoke" :weight bold :height 1.2 :family "Sans Serif"))))
 '(helm-ff-symlink ((t (:foreground "peach puff"))))
 '(helm-ff-directory ((t (:inherit helm-ff-file :height 1.2 :slant italic :weight ultra-bold))))
 '(helm-ff-invalid-symlink ((t (:foreground "red"))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-executable ((t (:foreground "spring green"))))
 '(helm-ff-dirs ((t (:inherit (font-lock-function-name-face)))))
 '(linum ((t (:background "gray5" :foreground "gray35"))))
 '(linum-highlight-face ((t (:inherit highlight))))
 '(helm-match ((t (:inverse-video t))))
 '(helm-separator ((((background dark)) (:foreground "red")) (((background light)) (:foreground "#ffbfb5"))))
 '(helm-selection ((t (:background "royal blue" :distant-foreground "white smoke"))))
 '(helm-selection-line ((t (:inherit (highlight)))))
 '(org-block-begin-line ((t (:background "gray10" :foreground "gray40" :height 0.7))))
 '(org-block-end-line ((t (:inherit org-block-begin-line :height 0.8))))
 '(org-block ((t (:background "gray10"))))
 '(flycheck-error-list-warning ((t (:inherit warning :foreground "DarkOrange"))))
 '(flycheck-error-list-info ((t (:inherit flycheck-error-list-error :foreground "blue1"))))
 '(flycheck-error-list-error ((t (:inherit error))))
 '(flycheck-error-list-checker-name ((t (:inherit shadow))))
 '(flycheck-error-list-line-number ((t (:inherit shadow))))
 '(flycheck-error-list-highlight ((t (:inherit (highlight)))))
 '(flycheck-error-list-column-number ((t (:inherit shadow))))
 '(flycheck-error-list-id ((t (:inherit (font-lock-type-face)))))
 '(flycheck-error ((t (:background "red1" :foreground "white smoke"))))
 '(flycheck-warning ((t (:background "DarkOrange" :foreground "white smoke"))))
 '(flycheck-info ((t (:background "blue1" :foreground "white smoke"))))
 '(error ((t (:foreground "red" :weight bold))))
 '(warning ((default (:weight bold)) (((class color) (min-colors 16)) (:foreground "DarkOrange")) (((class color)) (:foreground "yellow"))))
 '(preproc-font-lock-preprocessor-background ((t (:inherit font-lock-preprocessor-face))))
 '(vhl/default-face ((t (:inherit default :inverse-video t))))
 '(helm-buffer-file ((t (:inherit helm-ff-file))))
 '(helm-grep-match ((((background light)) (:foreground "#b00000")) (((background dark)) (:foreground "gold1"))))
 '(helm-buffer-saved-out ((t (:inherit helm-buffer-file :underline (:color "red" :style wave)))))
 '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
 '(helm-buffer-not-saved ((t (:inherit warning))))
 '(helm-buffer-process ((t (:inherit shadow))))
 '(helm-buffer-size ((t (:inherit shadow))))
 '(helm-helper ((t (:inherit (helm-header)))))
 '(hl-indent-block-face-1 ((t (:background "gray10"))))
 '(hl-indent-block-face-2 ((t (:background "gray13"))))
 '(hl-indent-block-face-3 ((t (:background "gray16"))))
 '(hl-indent-block-face-4 ((t (:background "gray19"))))
 '(hl-indent-block-face-5 ((t (:background "gray22"))))
 '(hl-indent-block-face-6 ((t (:background "gray25"))))
 '(writegood-duplicates-face ((t (:inherit (flyspell-duplicate)))))
 '(writegood-passive-voice-face ((t (:inherit (flyspell-duplicate)))))
 '(writegood-weasels-face ((t (:inherit (flyspell-incorrect)))))
 '(hl-line ((t (:inherit highlight))))
 '(info ((t (:inherit warning :foreground "light green"))))
 '(hl-todo-info ((t (:inherit info :inverse-video t :weight ultra-bold :height 1.3))))
 '(hl-todo-warning ((t (:inherit warning :inverse-video t :weight ultra-bold :height 1.3))))
 '(hl-todo-error ((t (:inherit error :inverse-video t :weight ultra-bold :height 1.3))))
 '(outline-1 ((t (:inherit default :weight ultra-bold :height 1.4))))
 '(outline-2 ((t (:inherit default :weight ultra-bold :height 1.4))))
 '(outline-3 ((t (:inherit default :weight ultra-bold :height 1.4))))
 '(outline-4 ((t (:inherit default :weight bold :height 1.2))))
 '(outline-5 ((t (:weight bold :inherit default :height 1.2))))
 '(outline-6 ((t (:weight bold :inherit default :height 1.2))))
 '(outline-7 ((t (:weight bold :inherit default :height 1.1))))
 '(outline-8 ((t (:weight bold :inherit default :height 1.1))))
 '(org-agenda-done ((t (:foreground "PaleGreen" :strike-through t :weight ultra-bold))))
 '(org-agenda-date ((t (:inherit helm-source-header))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :weight light))))
 '(org-headline-done ((t (:strike-through t :inherit shadow))))
 '(org-agenda-structure ((t (:inherit org-agenda-date))))
 '(company-preview ((t (:foreground "gray60"))))
 '(company-preview-common ((t (:inherit company-preview :weight bold))))
 '(company-tooltip ((t (:background "gray20" :foreground "white smoke"))))
 '(company-tooltip-common ((t (:inherit (company-preview-common company-tooltip)))))
 '(company-tooltip-selection ((t (:inherit helm-selection))))
 '(company-tooltip-common-selection ((t (:inherit (company-tooltip-common company-tooltip-selection)))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip))))
 '(company-scrollbar-fg ((t (:background "white smoke"))))
 '(org-date ((t (:background "RoyalBlue4" :foreground "white smoke"))))
 '(ediff-even-diff-Ancestor ((t (:inherit ediff-odd-diff-Ancestor))))
 '(ediff-odd-diff-A ((t (:background "coral1" :distant-foreground "white smoke"))))
 '(ediff-fine-diff-Ancestor ((t (:background "RoyalBlue3"))))
 '(ediff-odd-diff-B ((t (:background "DarkSeaGreen1" :distant-foreground "black"))))
 '(ediff-odd-diff-C ((((type pc)) (:background "gray40" :foreground "yellow3")) (((class color) (min-colors 88)) (:background "Grey")) (((class color) (min-colors 16)) (:background "Grey" :foreground "White")) (((class color)) (:weight bold :background "black" :foreground "yellow3")) (t (:stipple "gray1" :italic t))))
 '(ediff-odd-diff-Ancestor ((t (:background "LightSkyBlue1" :distant-foreground "white smoke"))))
 '(ediff-current-diff-Ancestor ((t (:background "SteelBlue1" :weight bold :inherit ediff-odd-diff-Ancestor))))
 '(ediff-fine-diff-A ((t (:inherit ediff-current-diff-A :background "firebrick1"))))
 '(ediff-fine-diff-B ((t (:inherit ediff-current-diff-B :background "forest green"))))
 '(ediff-fine-diff-C ((t (:inherit ediff-current-diff-C :background "#aaaa22"))))
 '(ediff-current-diff-A ((t (:inherit ediff-odd-diff-A :background "tomato3" :weight bold))))
 '(ediff-current-diff-B ((t (:inherit ediff-odd-diff-B :background "SeaGreen1" :weight bold))))
 '(ediff-current-diff-C ((((class color) (min-colors 88) (background light)) (:background "#ffffaa")) (((class color) (min-colors 88) (background dark)) (:background "#888833")) (((class color) (min-colors 16)) (:background "Pink" :foreground "Navy")) (((class color)) (:weight bold :background "yellow3" :foreground "cyan3")) (t (:inverse-video t))))
 '(ediff-even-diff-A ((t (:inherit ediff-odd-diff-A))))
 '(ediff-even-diff-B ((t (:inherit ediff-odd-diff-B))))
 '(ediff-even-diff-C ((t (:inherit ediff-odd-diff-C))))
 '(flycheck-okay ((t (:foreground "black" :background "light green" :inherit flycheck-error))))
 '(vr/match-separator-face ((((class color)) (:bold t :foreground "red")) (t (:inverse-video t))))
 '(vr/match-0 ((((class color) (background light)) (:background "lightblue")) (((class color) (background dark)) (:background "steelblue4")) (t (:inverse-video t))))
 '(vr/match-1 ((((class color) (background light)) (:background "pale turquoise")) (((class color) (background dark)) (:background "dodgerblue4")) (t (:inverse-video t))))
 '(vr/group-0 ((((class color) (background light)) (:background "aquamarine")) (((class color) (background dark)) (:background "blue3")) (t (:inverse-video t))))
 '(vr/group-1 ((((class color) (background light)) (:background "springgreen")) (((class color) (background dark)) (:background "chartreuse4")) (t (:inverse-video t))))
 '(vr/group-2 ((((min-colors 88) (class color) (background light)) (:background "yellow1")) (((class color) (background light)) (:background "yellow")) (((class color) (background dark)) (:background "sienna4")) (t (:inverse-video t))))
 '(num3-face-even ((t nil)))
 '(num3-face-odd ((t (:inverse-video t))))
 '(magit-branch-current ((t (:inherit magit-branch-local :inverse-video t))))
 '(default ((t (:inherit nil :stipple nil :background "gray5" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-bold :height 100 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(vertical-border ((t (:inherit default :inverse-video t)))))

(provide-theme 'cpp-entwickler.de)
