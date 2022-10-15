;;  *temp*
;;
;; Date: 2022-09-04
;; Author: Martin Schön <martin.schoen@shark-bytes.de>
;; Copyright © SharkBytes GmbH. All rights reserved.

;;  *temp*
;;
;; Date: 2022-09-04
;; Author: Martin Schön <martin.schoen@shark-bytes.de>
;; Copyright © SharkBytes GmbH. All rights reserved.

;;  *temp*
;;
;; Date: 2022-08-27
;; Author: Martin Schön <martin.schoen@shark-bytes.de>
;; Copyright © SharkBytes GmbH. All rights reserved.

;;  *temp*
;;
;; Date: 2022-08-27
;; Author: Martin Schön <martin.schoen@shark-bytes.de>
;; Copyright © SharkBytes GmbH. All rights reserved.

;;  *temp*
;;
;; Date: 2022-08-27
;; Author: Martin Schön <martin.schoen@shark-bytes.de>
;; Copyright © SharkBytes GmbH. All rights reserved.

(deftheme shark-bytes
  "Created 2022-04-09.")

(custom-theme-set-faces
 'shark-bytes
 `(avy-background-face ((t (:foreground ,shark-bytes-shadow :background ,shark-bytes-window-background :extend t))))
 `(avy-lead-face ((t (:foreground ,shark-bytes-text :background ,shark-bytes-cozy))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-1 ((t (:inherit avy-lead-face))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face))))
 `(blamer-face ((t (:slant italic :weight bold :height 0.4))))
 '(button ((t (:inherit link :box (:line-width 1 :style released-button)))))
 '(completions-annotations ((t (:inherit shark-bytes-ui :height 0.8 :weight thin :inherit italic))))
 '(corfu-annotations ((t (:inherit completions-annotations))))
 `(corfu-bar ((t (:background ,shark-bytes-text))))
 '(corfu-border ((t)))
 '(corfu-current ((t (:inherit hl-line))))
 '(corfu-default ((t (:inherit shark-bytes-minibuffer-active))))
 '(corfu-deprecated ((t (:inherit corfu-default :weight light))))
 '(corfu-echo ((t (:inherit corfu-default))))
 '(cov-coverage-not-run-face ((t (:inherit error))))
 '(cov-coverage-run-face ((t (:inherit info))))
 `(shark-bytes-minibuffer-active ((t (:inherit shark-bytes-ui :background ,shark-bytes-menu-window-background))))
 '(shark-bytes-minibuffer-header ((t (:inherit minibuffer-active :weight bold :height 1.3))))
 '(shark-bytes-mode-line-de-emphasis ((t (:inherit mode-line :height 0.7 :weight extra-light :slant italic))))
 '(shark-bytes-mode-line-emphasis ((t (:inherit mode-line :weight bold))))
 '(shark-bytes-sidebar ((t (:inherit shark-bytes-ui))))
 '(shark-bytes-ui ((t (:family "Noto Sans Medium"))))
 `(cursor ((t (:background ,shark-bytes-text :extend t))))
 '(dashboard-footer ((t (:inherit shark-bytes-ui :height 1.1 :slant italic))))
 '(dashboard-heading ((t (:inherit shark-bytes-ui-section))))
 '(dashboard-navigator ((t (:inherit shark-bytes-ui))))
 '(dashboard-items-face ((t (:inherit shark-bytes-ui))))
 `(dashboard-no-items-face ((t (:inherit dashboard-items-face :foreground ,shark-bytes-shadow))))
 `(default ((t (:inherit nil :stipple nil :background ,shark-bytes-window-background :foreground ,shark-bytes-text :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width condensed :family "FuraCode Nerd Font"))))
 '(diff-refine-added ((t (:inherit magit-diff-added-highlight :weight ultra-bold :inverse-video t))))
 '(diff-refine-removed ((t (:inherit magit-diff-removed-highlight :weight ultra-bold :inverse-video t))))
 `(error ((t (:foreground ,shark-bytes-error :distant-foreground ,shark-bytes-error-saturated :weight bold))))
 '(escape-glyph ((t (:foreground "#8D6E63"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 `(flycheck-error ((t (:inherit error :background ,shark-bytes-shadow :inverse-video t :height 1.0))))
 '(flycheck-error-list-checker-name ((t (:inherit shadow))))
 '(flycheck-error-list-column-number ((t (:inherit shadow))))
 '(flycheck-error-list-error ((t (:inherit error))))
 '(flycheck-error-list-highlight ((t (:inherit (highlight)))))
 '(flycheck-error-list-id ((t (:foreground "#803AF6"))))
 '(flycheck-error-list-info ((t (:inherit info))))
 '(flycheck-error-list-line-number ((t (:inherit shadow))))
 '(flycheck-error-list-warning ((t (:inherit warning))))
 `(flycheck-info ((t (:inherit info :background ,shark-bytes-shadow :inverse-video t :height 1.0))))
 `(flycheck-warning ((t (:inherit warning :background ,shark-bytes-shadow :inverse-video t :height 1.0))))
 '(font-lock-builtin-face ((t (:foreground "#AC59D9" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:inherit shadow))))
 '(font-lock-comment-face ((t (:foreground "#C953BF" :slant italic :height 0.9))))
 '(font-lock-constant-face ((t (:inherit font-lock-variable-name-face :underline t))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:inherit default))))
 '(font-lock-keyword-face ((t (:foreground "#9A65F6" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#C97053" :weight ultra-bold))))
 '(font-lock-preprocessor-face ((t (:inherit default :slant italic :weight ultra-light))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#C98A53" :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-string-face ((t (:foreground "#C9BD53"))))
 '(font-lock-type-face ((t (:inherit default :width extra-condensed :weight extra-light))))
 '(font-lock-variable-name-face ((t (:inherit default :weight ultra-bold))))
 '(font-lock-warning-face ((t (:inherit warning))))
 '(lsp-face-semhl-namespace ((t (:inherit font-lock-function-name-face :weight extra-light :slant italic))))
 `(fringe ((t (:background ,shark-bytes-window-background))))
 '(git-gutter-fr:modified ((t (:inherit shadow))))
 '(git-gutter-fr:added ((t (:inherit git-gutter-fr:modified))))
 '(git-gutter-fr:deleted ((t (:inherit git-gutter-fr:modified))))
 '(header-line ((t (:inherit mode-line))))
 `(highlight ((t (:background ,shark-bytes-highlight))))
 `(hl-indent-scope-odd-face ((t (:background ,shark-bytes-window-background))))
 `(hl-indent-scope-even-face ((t (:background ,shark-bytes-block-background))))
 `(hl-line ((t (:background ,shark-bytes-current-line))))
 '(hl-todo-error ((t (:inherit error :inverse-video t :weight ultra-bold :height 1.3))))
 '(hl-todo-info ((t (:inherit info :inverse-video t :weight ultra-bold :height 1.3))))
 '(hl-todo-warning ((t (:inherit warning :inverse-video t :weight ultra-bold :height 1.3))))
 `(info ((t (:inherit warning :foreground ,shark-bytes-info))))
 '(isearch ((t (:inherit default))))
 '(isearch-fail ((t (:inherit error :background "#FEFEFE" :inverse-video t))))
 '(italic ((t (:inherit default :slant italic))))
 '(kubernetes-context-name ((t (:inherit magit-branch-local))))
 '(lazy-highlight ((t (:background "#9FA8DA"))))
 `(line-number ((t (:family "Noto Sans Mono Medium" :height 0.9 :foreground ,shark-bytes-current-line))))
 `(line-number-current-line ((t (:inherit line-number :foreground ,shark-bytes-highlight))))
 '(link ((t (:inherit shark-bytes-ui :underline t :weight bold))))
 '(link-visited ((t (:inherit link :weight light))))
 `(lsp-ui-doc-background ((t (:inherit default :background ,shark-bytes-menu-window-background))))
 `(magit-bisect-bad ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-error))))
 `(magit-bisect-good ((t (:inherit magit-bisect-bad :foreground ,shark-bytes-ok))))
 `(magit-bisect-skip ((t (:inherit magit-bisect-bad :foreground ,shark-bytes-shadow))))
 `(magit-branch-current ((t (:inherit magit-branch-local :foreground ,shark-bytes-burning :weight bold))))
 `(magit-branch-local ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-hot))))
 `(magit-branch-remote ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-chilly))))
 '(magit-branch-remote-head ((t (:inherit magit-branch-remote :slant italic))))
 '(magit-branch-upstream ((t (:inherit magit-branch-remote :weight bold))))
 `(magit-cherry-equivalent ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-freezing))))
 `(magit-cherry-unmatched ((t (:inherit magit-cherry-equivalent :foreground ,shark-bytes-burning))))
 `(magit-diff-added ((t (:inherit magit-diff-context-highlight :foreground ,shark-bytes-freezing))))
 '(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
 `(magit-diff-base ((t (:inherit magit-diff-context-highlight :foreground ,shark-bytes-cozy))))
 '(magit-diff-base-highlight ((t (:inherit magit-diff-base))))
 '(magit-diff-conflict-heading ((t (:inherit magit-diff-hunk-heading))))
 `(magit-diff-context-highlight ((t (:inherit default :background ,shark-bytes-block-background :extend t))))
 '(magit-diff-file-heading ((t (:inherit shark-bytes-ui :extend t))))
 '(magit-diff-file-heading-highlight ((t (:inherit magit-diff-file-heading))))
 '(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight))))
 `(magit-diff-hunk-heading ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-shadow :background ,shark-bytes-menu-window-background :extend t))))
 `(magit-diff-hunk-heading-highlight ((t (:inherit magit-diff-hunk-heading :foreground ,shark-bytes-text))))
 `(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :weight bold))))
 '(magit-diff-hunk-region ((t (:extend t :inherit (bold)))))
 '(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading))))
 '(magit-diff-our ((t (:inherit magit-diff-removed))))
 '(magit-diff-our-highlight ((t (:inherit magit-diff-removed-highlight))))
 `(magit-diff-removed ((t (:inherit magit-diff-context-highlight :foreground ,shark-bytes-burning))))
 '(magit-diff-removed-highlight ((t (:inherit magit-diff-removed :weight bold))))
 '(magit-diff-revision-summary ((t (:inherit magit-diff-hunk-heading))))
 '(magit-diff-revision-summary-highlight ((t (:inherit magit-diff-hunk-heading-highlight))))
 '(magit-diff-their ((t (:inherit magit-diff-added))))
 '(magit-diff-their-highlight ((t (:inherit magit-diff-added-highlight))))
 '(magit-diff-whitespace-warning ((t (:inherit trailing-whitespace))))
 '(magit-diffstat-added ((t (:inherit magit-diff-added))))
 '(magit-diffstat-removed ((t (:inherit magit-diff-removed))))
 '(magit-filename ((t (:inherit shark-bytes-ui))))
 `(magit-hash ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-insignificant))))
 '(magit-head ((t (:inherit magit-branch-local :slant italic))))
 '(magit-header-line ((t (:inherit magit-section-heading))))
 '(magit-header-line-key ((t (:inherit (font-lock-builtin-face)))))
 '(magit-header-line-log-select ((t (:inherit (bold)))))
 '(magit-keyword ((t (:inherit (font-lock-string-face)))))
 '(magit-keyword-squash ((t (:inherit (font-lock-warning-face)))))
 `(magit-log-author ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-minor))))
 `(magit-log-date ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-detail))))
 `(magit-log-graph ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-detail))))
 `(magit-popup-argument ((t (:inherit (font-lock-warning-face)))))
 '(magit-popup-disabled-argument ((t (:inherit (shadow)))))
 '(magit-popup-heading ((t (:inherit (font-lock-keyword-face)))))
 '(magit-popup-key ((t (:inherit (font-lock-builtin-face)))))
 '(magit-popup-option-value ((t (:inherit (font-lock-string-face)))))
 `(magit-process-ng ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-error))))
 `(magit-process-ok ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-ok))))
 `(magit-reflog-amend ((t (:inherit magit-reflog-other :foreground ,shark-bytes-warm))))
 `(magit-reflog-checkout ((t (:inherit magit-reflog-other :foreground ,shark-bytes-freezing))))
 `(magit-reflog-cherry-pick ((t (:inherit magit-reflog-other :foreground ,shark-bytes-warm))))
 `(magit-reflog-commit ((t (:inherit magit-reflog-other :foreground ,shark-bytes-mild))))
 `(magit-reflog-merge ((t (:inherit magit-reflog-other :foreground ,shark-bytes-hot))))
 '(magit-reflog-other ((t (:inherit shark-bytes-ui))))
 `(magit-reflog-rebase ((t (:inherit magit-reflog-other :foreground ,shark-bytes-burning))))
 `(magit-reflog-remote ((t (:inherit magit-reflog-other :foreground ,shark-bytes-cold))))
 `(magit-reflog-reset ((t (:inherit magit-reflog-other :foreground ,shark-bytes-burning))))
 '(magit-refname ((((class color) (background light)) (:foreground "grey30")) (((class color) (background dark)) (:foreground "grey80"))))
 '(magit-refname-pullreq ((t (:inherit (magit-refname)))))
 '(magit-refname-stash ((t (:inherit (magit-refname)))))
 '(magit-refname-wip ((t (:inherit (magit-refname)))))
 '(magit-section-heading ((t (:inherit shark-bytes-ui-section))))
 '(magit-section-heading-selection ((t (:inherit magit-section-heading))))
 '(magit-section-highlight ((t (:inherit (:inherit hl-line)))))
 '(magit-section-secondary-heading ((t (:inherit shark-bytes-ui-subsection))))
 '(magit-sequence-done ((t (:inherit (magit-hash)))))
 '(magit-sequence-drop ((((class color) (background light)) (:foreground "IndianRed")) (((class color) (background dark)) (:foreground "IndianRed"))))
 '(magit-sequence-exec ((t (:inherit (magit-hash)))))
 '(magit-sequence-head ((((class color) (background light)) (:foreground "SkyBlue4")) (((class color) (background dark)) (:foreground "LightSkyBlue1"))))
 '(magit-sequence-onto ((t (:inherit (magit-sequence-done)))))
 '(magit-sequence-part ((((class color) (background light)) (:foreground "Goldenrod4")) (((class color) (background dark)) (:foreground "LightGoldenrod2"))))
 '(magit-sequence-pick ((t (:inherit (default)))))
 '(magit-sequence-stop ((((class color) (background light)) (:foreground "DarkOliveGreen4")) (((class color) (background dark)) (:foreground "DarkSeaGreen2"))))
 '(magit-signature-bad ((t (:inherit error))))
 '(magit-signature-error ((t (:inherit magit-signature-bad))))
 '(magit-signature-expired ((t (:inherit warning))))
 '(magit-signature-expired-key ((t (:inherit warning))))
 `(magit-signature-good ((t (:inherit error :foreground ,shark-bytes-ok))))
 '(magit-signature-revoked ((t (:inherit warning))))
 '(magit-signature-untrusted ((t (:inherit warning))))
 `(magit-tag ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-freezing :weight bold))))
 '(marginalia-documentation ((t (:inherit completions-annotations))))
 '(match ((t (:background "#3F51B5"))))
 '(minibuffer-prompt ((t (:inherit shark-bytes-ui :weight bold :height 1.2))))
 `(mode-line ((t (:inherit shark-bytes-ui :background ,shark-bytes-mild :height 1.2 :extend t))))
 '(mode-line-highlight ((t (:inherit mode-line))))
 `(mode-line-inactive ((t (:inherit mode-line :background ,shark-bytes-menu-window-background :weight light))))
 '(next-error ((t (:inherit match))))
 '(num3-face-even ((t (:underline t))))
 '(num3-face-odd ((t nil)))
 '(org-agenda-clocking ((t (:weight ultra-bold :background "gray20" :inherit default))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :weight light))))
 '(org-agenda-done ((t (:foreground "#92C953" :strike-through t :weight ultra-bold))))
 '(org-agenda-structure ((t (:inherit org-agenda-date :height 1.5 :weight bold))))
 `(org-block ((t (:background ,shark-bytes-block-background))))
 '(org-block-begin-line ((t (:inherit org-block :height 0.7 :weight extra-light :foreground "#7A7A7A"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-date ((t (:background "#803AF6" :foreground "#FAFAFA"))))
 '(org-ellipsis ((t nil)))
 '(org-headline-done ((t (:strike-through t :inherit shadow))))
 '(org-link ((t (:inherit link))))
 '(org-priority-high ((t (:inherit error))))
 '(org-priority-low ((t (:inherit info))))
 '(org-priority-medium ((t (:inherit warning))))
 '(outline-1 ((t (:inherit default :weight ultra-bold :height 1.4))))
 '(outline-2 ((t (:inherit default :weight ultra-bold :height 1.3))))
 '(outline-3 ((t (:height 1.2 :weight ultra-bold :inherit default))))
 '(outline-4 ((t (:height 1.1 :weight bold :inherit default))))
 '(outline-5 ((t (:weight bold :inherit default :height 1.2))))
 '(outline-6 ((t (:weight bold :inherit default :height 1.2))))
 '(outline-7 ((t (:weight bold :inherit default :height 1.1))))
 '(outline-8 ((t (:weight bold :inherit default :height 1.1))))
 '(query-replace ((t (:inherit match))))
 '(rainbow-delimiters-base-face ((t)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#473EF5" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#803AF6" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#C936BB" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#C95C36" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#C97D36" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#C9B136" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#92C953" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#36C999" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#369DC9" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit (error rainbow-delimiters-base-face) :inverse-video t))))
 '(region ((t (:background "#9A65F6" :distant-foreground "white smoke"))))
 '(secondary-selection ((t (:background "#8D68FD" :inherit region))))
 '(shadow ((t (:foreground "#757575"))))
 '(tooltip ((t (:inherit default))))
 `(trailing-whitespace ((t (:background ,shark-bytes-warning))))
 '(transient-heading ((t (:inherit shark-bytes-sidebar-section))))
 '(transient-key ((t (:inherit shark-bytes-ui :height 1.3))))
 '(transient-argument ((t (:inherit shark-bytes-ui :height 0.9 :weight light))))
 `(transient-value ((t (:inherit transient-inactive-value :foreground ,shark-bytes-cozy))))
 '(transient-inactive-argument ((t (:inherit transient-argument))))
 '(transient-inactive-value ((t (:inherit shark-bytes-ui))))
 `(transient-unreachable ((t (:inherit shark-bytes-ui :foreground ,shark-bytes-shadow))))
 '(transient-active-infix ((t (:inherit secondary-selection))))
 `(transient-unreachable-key ((t (:inherit transient-key :foreground ,shark-bytes-shadow))))
 '(transient-nonstandard-key ((t (:underline t))))
 `(transient-mismatched-key ((t (:inherit transient-key :foreground ,shark-bytes-warning))))
 '(transient-inapt-suffix ((t (:inherit shadow :italic t))))
 '(transient-enabled-suffix ((t (:inherit shark-bytes-ui :weight bold))))
 '(transient-disabled-suffix ((t (:inherit transient-enabled-suffix))))
 '(transient-higher-level ((t (:underline t))))
 '(transient-separator ((t nil)))
 '(variable-pitch ((((type w32)) (:font "-outline-Arial-normal-normal-normal-sans-*-*-*-*-p-*-iso8859-1")) (t (:family "Sans Serif"))))
 '(vertical-border ((t (:inherit default :inverse-video t))))
 '(vr/group-0 ((t (:inverse-video t :inherit rainbow-delimiters-depth-6-face))))
 '(vr/group-1 ((t (:inverse-video t :inherit rainbow-delimiters-depth-7-face))))
 '(vr/group-2 ((t (:inverse-video t :inherit rainbow-delimiters-depth-8-face))))
 '(vr/match-0 ((t (:inverse-video t :inherit rainbow-delimiters-depth-1-face))))
 '(vr/match-1 ((t (:inverse-video t :inherit rainbow-delimiters-depth-2-face))))
 '(vr/match-separator-face ((t (:foreground "#FAFAFA" :weight ultra-bold))))
 `(warning ((t (:foreground ,shark-bytes-warning :distant-foreground ,shark-bytes-warning-saturated :weight bold))))
 )

(provide-theme 'shark-bytes)