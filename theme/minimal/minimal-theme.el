;;; minimal-theme.el --- A light/dark minimalistic Emacs 24 theme.

;; Copyright (C) 2014 Anler Hp

;; Author: Anler Hp <anler86 [at] gmail.com>
;; Keywords: color, theme, minimal
;; X-URL: http://github.com/ikame/minimal-theme
;; URL: http://github.com/ikame/minimal-theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A minimalistic color theme to avoid distraction with
;; colors. Based on monochrome theme.

;;; Code:
(deftheme minimal "minimal dark theme.")

(let* ((class '((class color) (min-colors 89)))
       (minimal-background  "#1D1F21")
       (minimal-foreground  "#C5C8C6")
       (minimal-selection   "#50545a")
       (minimal-comment     "#707880")
       (minimal-red         "#CC6666")
       (minimal-orange      "#DE935F")
       (minimal-yellow      "#F0C674")
       (minimal-green       "#B5BD68")
       (minimal-aqua        "#8ABEB7")
       (minimal-blue        "#81A2BE")
       (minimal-purple      "#B294BB")

       (foreground "grey90")
       (background "grey10")
       (cursor "white")
       (border "grey10")
       (minibuffer cursor)
       (region  "grey40")
       (secondary-region "grey15")
       (comment-delimiter "grey16")
       (comment "grey35")
       (constant foreground)
       (string "grey80")
       (modeline-foreground foreground)
       (modeline-background "grey10")
       (modeline-foreground-inactive foreground)
       (modeline-background-inactive "grey20")
       (hl-background region)
       (hl-face-background nil)
       (failure "red")
       (org-background "grey8")
       )
  (setq fci-rule-color comment)

  (custom-theme-set-faces
   'minimal

   ;; basic stuff
   `(default ((,class (:background ,background :foreground ,foreground))))
   `(cursor ((,class (:background ,cursor :inverse-video t))))
   `(vertical-border ((,class (:foreground ,border))))

   ;; minibuffer
   `(minibuffer-prompt ((,class (:foreground ,minibuffer :weight bold))))

   ;; region
   `(region ((,class (:foreground ,background :background ,region))))
   `(secondary-selection ((,class (:background ,secondary-region))))

   `(trailing-whitespace ((,class (:background ,minimal-yellow))))

   ;; faces
   `(font-lock-builtin-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,foreground :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))

   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-delimiter))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-doc-face ((,class (:inherit (font-lock-comment-face)))))
   `(font-lock-string-face ((,class (:foreground ,foreground :foreground ,string))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,foreground :background ,region :weight normal))))
   `(isearch-fail ((,class (:foreground ,failure :bold t))))
   `(lazy-highlight ((,class (:foreground ,foreground :background ,secondary-region))))

   ;; ido-mode
   `(ido-subdir ((,class (:foreground ,foreground :weight bold))))
   `(ido-only-match ((,class (:foreground ,foreground :weight bold))))

   ;; show-paren
   `(show-paren-match
     ((,class (:background ,region))))
   `(show-paren-mismatch
     ((,class (:foreground ,failure :weight bold))))

   ;; modeline
   `(mode-line
     ((,class (:foreground ,modeline-foreground
                           :background ,modeline-background
                           :box (:line-width 1 :color ,background :style unspecified)
                           ;; :box nil
                           ))))
   ;; `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,modeline-foreground-inactive
                           :background ,modeline-background-inactive
                           :box (:line-width 1 :color ,border :style unspecified)
                           ;; :box nil
                           ))))

   ;; Smart Mode Line
   `(sml/global ((,class (:foreground ,foreground))))
   `(sml/line-number ((,class (:weight bold :foreground ,foreground))))
   `(sml/col-number ((,class (:foreground ,foreground))))
   `(sml/filename ((,class (:weight bold :foreground ,foreground))))
   `(sml/folder ((,class (:weight bold :foreground ,minimal-purple))))
   `(sml/git ((,class (:weight bold :foreground ,minimal-blue))))
   `(sml/modes ((,class (:foreground ,minimal-blue))))
   `(sml/modified ((,class (:foreground ,minimal-red))))
   `(sml/mule-info ((,class (:foreground ,minimal-aqua))))
   `(sml/not-modified ((,class (:foreground ,foreground))))
   `(sml/numbers-separator ((,class (:foreground ,foreground))))
   `(sml/outside-modified ((,class (:foreground ,minimal-red))))
   `(sml/position-percentage ((,class (:foreground ,minimal-yellow))))
   `(sml/prefix ((,class (:weight bold :foreground ,minimal-green))))
   `(sml/process ((,class (:weight bold :foreground ,minimal-green))))
   `(sml/read-only ((,class (:foreground ,minimal-blue))))
   `(sml/sudo ((,class (:foreground ,minimal-red))))
   `(sml/time ((,class (:foreground ,foreground))))
   `(sml/vc ((,class (:foreground ,minimal-blue))))
   `(sml/vc-edited ((,class (:foreground ,minimal-red))))
   `(sml/warning ((,class (:weight bold :foreground ,minimal-red))))

   ;; hl-line-mode
   `(hl-line ((,class (:background ,hl-background))))
   `(hl-line-face ((,class (:background ,hl-face-background))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,foreground
                           :background ,background
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,foreground
                           :background nil
                           :weight bold
                           :underline ,foreground
                           :box nil))))

   `(helm-selection ((,class (:background ,foreground :foreground ,background :underline nil))))
   ;; `(helm-selection-line ((,class (:background ,smyx-bg+1))))
   ;; `(helm-visible-mark ((,class (:foreground ,smyx-bg :background ,smyx-yellow-2))))
   `(helm-candidate-number ((,class (:weight bold :foreground ,modeline-foreground :background ,modeline-background))))

   `(helm-ff-directory ((,class (:weight bold :foreground ,foreground :background nil))))
   ;; `(helm-ff-executable ((,class (:foreground ,smyx-green))))
   `(helm-ff-file ((,class (:foreground ,foreground))))
   ;; `(helm-ff-symlink ((,class (:foreground ,smyx-blue))))
   ;; `(helm-ff-prefix ((,class (:weight bold :foreground ,smyx-yellow :background nil))))
   ;; `(helm-ff-invalid-symlink ((,class (:foreground ,smyx-gray :background ,smyx-red))))

   ;; company-mode
   `(company-tooltip ((,class (:background ,background :foreground ,foreground))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,comment))))
   `(company-tooltip-selection ((,class (:foreground ,background :background ,foreground))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,comment))))
   `(company-tooltip-annotation ((,class (:foreground ,foreground :background ,background))))

   `(company-scrollbar-fg ((,class (:background ,region))))
   `(company-scrollbar-bg ((,class (:background ,foreground))))
   `(company-preview ((,class (:foreground ,failure :background nil))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,minimal-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,foreground))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,minimal-yellow))))
   `(erc-keyword-face ((,class (:foreground ,minimal-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,minimal-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,minimal-red :weight bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,minimal-green))))
   `(erc-pal-face ((,class (:foreground ,minimal-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,minimal-orange :background ,background :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,minimal-green))))
   `(erc-underline-face ((t (:underline t))))

   ;; org-mode
   `(org-level-1 ((,class (:foreground ,foreground :height 1.6))))
   `(org-level-2 ((,class (:foreground ,foreground :height 1.5))))
   `(org-level-3 ((,class (:foreground ,foreground :height 1.4))))
   `(org-level-4 ((,class (:foreground ,foreground :height 1.3))))
   `(org-level-5 ((,class (:foreground ,foreground :height 1.2))))
   `(org-level-6 ((,class (:foreground ,foreground :height 1.1))))
   `(org-level-7 ((,class (:foreground ,foreground))))
   `(org-level-8 ((,class (:foreground ,foreground))))

   ;; outline
   `(outline-1 ((,class (:inherit org-level-1))))
   `(outline-2 ((,class (:inherit org-level-2))))
   `(outline-3 ((,class (:inherit org-level-3))))
   `(outline-4 ((,class (:inherit org-level-4))))
   `(outline-5 ((,class (:inherit org-level-5))))
   `(outline-6 ((,class (:inherit org-level-6))))
   `(outline-7 ((,class (:inherit org-level-7))))
   `(outline-8 ((,class (:inherit org-level-8))))

   `(org-document-title ((,class (:foreground ,foreground))))

   `(org-link ((,class (:background ,org-background :foreground ,foreground :underline t))))
   `(org-tag ((,class (:background ,org-background :foreground ,foreground))))
   `(org-warning ((,class (:background ,region :foreground ,foreground :weight bold))))
   `(org-todo ((,class (:background ,region :foreground ,foreground :weight bold))))
   `(org-done ((,class (:background ,region :foreground ,foreground :weight bold))))

   `(org-table ((,class (:background ,org-background))))
   `(org-code ((,class (:background ,org-background))))
   `(org-date ((,class (:background ,org-background :underline t))))
   `(org-block ((,class (:background ,org-background))))
   `(org-block-background ((,class (:background ,org-background :foreground ,foreground))))
   `(org-block-begin-line
     ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))
   `(org-block-end-line
     ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))

   ;; js2-mode
   `(js2-external-variable ((,class (:inherit base-faces :weight bold))))
   `(js2-function-param ((,class (:inherit base-faces))))
   `(js2-instance-member ((,class (:inherit base-faces))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:inherit base-faces))))
   `(js2-jsdoc-html-tag-name ((,class (:inherit base-faces))))
   `(js2-jsdoc-tag ((,class (:inherit base-faces))))
   `(js2-jsdoc-type ((,class (:inherit base-faces :weight bold))))
   `(js2-jsdoc-value ((,class (:inherit base-faces))))
   `(js2-magic-paren ((,class (:underline t))))
   `(js2-private-function-call ((,class (:inherit base-faces))))
   `(js2-private-member ((,class (:inherit base-faces))))

   ;; ansi-term
   `(term-color-white ((,class (:foreground ,foreground))))
   `(term-color-black ((,class (:foreground ,background))))
   `(term-color-red ((,class (:foreground ,minimal-red))))
   `(term-color-green ((,class (:foreground ,minimal-green))))
   `(term-color-yellow ((,class (:foreground ,minimal-yellow))))
   `(term-color-blue ((,class (:foreground ,minimal-blue))))
   `(term-color-magenta ((,class (:foreground ,minimal-purple))))
   `(term-color-cyan ((,class (:foreground ,minimal-aqua))))

   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'minimal)
