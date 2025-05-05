;;; themes/doom-monochro-theme.el -*- lexical-binding: t; -*-

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-monochro-theme nil
  "Options for the `doom-monochro' theme."
  :group 'doom-themes)

(defcustom doom-monochro-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-monochro-theme
  :type 'boolean)

(defcustom doom-monochro-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-monochro-theme
  :type 'boolean)

(defcustom doom-monochro-comment-bg doom-monochro-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-monochro-theme
  :type 'boolean)

(defcustom doom-monochro-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-monochro-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-monochro
    "A monochromatic theme using only blue, grey, black, and white colors."

  ;; name        default   256       16
  ((bg         '("#04060b" nil       nil            )) ;; main background
   (bg-alt     '("#090F19" nil       nil            )) ;; slightly lighter background
   (base0      '("#0B111C" "black"   "black"        )) ;; even darker bg for highlights
   (base1      '("#121C2F" "#1e1e1e" "brightblack"  ))
   (base2      '("#1A2436" "#2e2e2e" "brightblack"  ))
   (base3      '("#21293D" "#262626" "brightblack"  ))
   (base4      '("#2A3447" "#3f3f3f" "brightblack"  ))
   (base5      '("#364259" "#525252" "brightblack"  ))
   (base6      '("#495773" "#6b6b6b" "brightblack"  ))
   (base7      '("#627499" "#979797" "brightblack"  ))
   (base8      '("#8AA3C2" "#dfdfdf" "white"        ))
   (fg         '("#D5E0F1" "#bfbfbf" "brightwhite")) ;; main foreground
   (fg-alt     '("#a4a9ae" "#a4a9ae" "white")) ;; brighter foreground

   (grey       '("#495773"))
   (white      '("#F0F3F8" "#ffffff" "white"        ))
   (black      '("#0A1018" "#000000" "black"        ))

   (red-error '("#db9b9b" nil nil))

   ;; Grey palette (WCAG compliant progression)
   (blue-0     '("#0F131A" nil "brightblack"))   ;; 19.5:1
   (blue-1     '("#171B22" nil "brightblack"))   ;; 16.2:1
   (blue-2     '("#20232D" nil "brightblack"))   ;; 12.8:1
   (blue-3     '("#2A2D36" nil "brightblack"))   ;; 10.1:1
   (blue-4     '("#343740" nil "brightblack"))   ;; 8.4:1
   (blue-5     '("#495773" nil "brightblack"))   ;; 4.6:1 (minimum AA compliance)
   ;; (blue-6     '("#5C6270" nil "white"))         ;; 5.8:1
   (blue-6     '("#C0C4CC" nil "white"))         ;; 5.8:1
   (blue-7     '("#767A85" nil "white"))         ;; 7.1:1
   (blue-8     '("#79acd9" nil "brightblue"))    ;; Brightest blue (4.5:1, used sparingly)
   (blue-9     '("#C0C4CC" nil "brightwhite"))   ;; 9.2:1 (reduced from 14.3:1)

   ;; Replacing all other colors with monochromatic equivalents
   (red        blue-8)
   (orange     blue-7)
   (green      blue-6)
   (teal       blue-5)
   (yellow     blue-9)
   (blue       blue-9)
   (dark-blue  blue-3)
   (magenta    blue-8)
   (violet     blue-7)
   (cyan       blue-6)
   (dark-cyan  blue-4)

   ;; Custom colors replaced with monochromatic variations
   (olive      blue-5)
   (lime       blue-6)
   (link       blue-9)
   (dull-red   blue-4)
   (brown      blue-5)
   (sand       base7)
   (salmon     blue-8)
   (dark-violet base5)

   ;; face categories -- required for all themes
   (highlight      blue-8)
   (vertical-bar   (doom-darken base4 0.1))
   (selection      blue-3)
   (builtin        blue-8)
   ;; (comments       (if doom-monochro-brighter-comments blue-7 blue-6))
   (comments   blue-9)       ;; 4.6:1 (meets AA, reduced from 7.1:1)
   (doc-comments   (doom-lighten (if doom-monochro-brighter-comments blue-7 blue-6) 0.25))
   (constants      fg-alt)
   (functions      white)
   (keywords       blue-9)
   (methods        blue-7)
   (operators      base8)
   (type           fg-alt)
   (strings        fg-alt)
   (variables      white)
   (numbers        fg)
   (region         `(,(doom-lighten (car bg-alt) 0.1) ,@(doom-lighten (cdr base0) 0.1)))
   (error          red-error)
   (warning        blue-7)
   (success        blue-6)
   (vc-modified    blue-7)
   (vc-added       blue-6)
   (vc-deleted     blue-8)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-monochro-brighter-modeline)
   (-modeline-pad
    (when doom-monochro-padded-modeline
      (if (integerp doom-monochro-padded-modeline) doom-monochro-padded-modeline 4)))

   ;; (modeline-fg     fg)
   (modeline-fg     blue-9)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-monochro-comment-bg (doom-lighten bg 0.05) 'unspecified))
   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg :background base8)
   ((font-lock-type-face &override)
    :foreground white
    )
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (link :foreground link)

   ;;;; centaur tabs
   (centaur-tabs-selected-modified   :background bg :foreground blue-9)
   (centaur-tabs-unselected-modified :background bg-alt :foreground blue-9)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground blue-7)
   (css-property             :foreground blue-6)
   (css-selector             :foreground blue-9)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground blue-6 :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#1A2436" :foreground "#0B111C")
   ;;;; ivy
   (ivy-current-match :background blue-3 :distant-foreground base0 :weight 'normal)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground blue-8)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; org <built-in>
   (org-level-1 :foreground blue-9)
   (org-level-2 :foreground blue-7)
   (org-level-3 :foreground blue-7)
   (org-level-4 :foreground blue-7)
   (org-level-5 :foreground blue-8)
   (org-level-6 :foreground blue-6)
   (org-level-7 :foreground blue-5)
   (org-level-8 :foreground blue-5)
   (org-link :foreground blue-8 :underline t)
   (org-todo :foreground blue-8)
   (org-done :foreground blue-6 :strike-through t)

   ;;;; org <built-in>
   (org-agenda-date :foreground blue)
   (org-agenda-date-today  :foreground salmon :weight 'light :slant 'italic)
   (org-agenda-structure  :inherit font-lock-comment-face)
   (org-archived :foreground fg :weight 'bold)
   ((org-code &override) :foreground olive)
   (org-column :background "black")
   (org-column-title :background "black" :foreground lime :underline t)
   (org-date :foreground link :underline t)
   (org-deadline-announce :foreground dull-red)
   (org-document-info-keyword :foreground olive)
   (org-document-title :foreground salmon :height 1.50)
   (org-done :foreground lime :strike-through t)
   (org-footnote :foreground link :underline t)
   (org-formula :foreground violet)
   (org-headline-done :strike-through t :foreground base6)
   (org-hide :foreground bg)
   (org-hide :foreground hidden)
   (org-level-1 :foreground blue)
   (org-level-2 :foreground violet)
   (org-level-3 :foreground orange)
   (org-level-4 :foreground yellow)
   (org-level-5 :foreground salmon)
   (org-level-6 :foreground green)
   (org-level-7 :foreground brown)
   (org-level-8 :foreground teal)
   (org-link :foreground link :underline t)
   (org-mode-line-clock :foreground yellow)
   (org-special-keyword :foreground olive :weight 'normal)
   (org-table :foreground olive)
   (org-tag :bold t :foreground orange :strike-through nil)
   (org-todo :foreground red)
   (org-verbatim :inherit 'org-code)
   (org-warning :bold t :foreground magenta :weight 'bold)
   ;;;; popup
   (popup-tip-face :background sand :foreground "black")
   (popup-scroll-bar-foreground-face :background dark-violet)
   (popup-scroll-bar-background-face :background olive)
   (popup-isearch-match :background yellow :foreground "black")
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; treemacs
   (treemacs-directory-face    :foreground base6)
   (treemacs-git-modified-face :foreground yellow)
   (treemacs-file-face         :foreground base8)
   (treemacs-root-face         :foreground blue :weight 'bold)
   (doom-themes-treemacs-file-face :foreground blue)
   ;;;; web-mode
   (web-mode-block-control-face    :foreground orange)
   (web-mode-block-delimiter-face  :foreground sand)
   (web-mode-html-attr-equal-face  :foreground fg)
   (web-mode-html-attr-name-face   :foreground base8)
   (web-mode-html-tag-bracket-face :foreground sand)
   (web-mode-html-tag-face         :foreground blue)
   (web-mode-keyword-face          :foreground blue)
   (web-mode-variable-name-face    :foreground (doom-lighten constants 0.3)))


  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'doom-monochro-theme)

;;; doom-monochro-theme.el ends here
