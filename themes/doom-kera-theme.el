;;; doom-kera-theme.el -*- lexical-binding: t; -*-

(require 'doom-themes)

(defgroup doom-kera-theme nil
  "Options for the `doom-kera' theme."
  :group 'doom-themes)

(defcustom doom-kera-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kera-theme
  :type 'boolean)

(defcustom doom-kera-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kera-theme
  :type 'boolean)

(defcustom doom-kera-comment-bg doom-kera-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-kera-theme
  :type 'boolean)

(defcustom doom-kera-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-kera-theme
  :type '(choice integer boolean))

(def-doom-theme doom-kera
    "A custom theme for keratoconus with subtle highlights"

  ;; name        default   256       16
  (
   (bg         '("#090F19" nil nil))          ; Universal background
   (bg-alt     '("#020406" nil nil))          ; Slightly lighter background
   (base0      '("#090F19" "black" "black"))  ; Darkest UI elements
   (base1      '("#080E17" "#080E17" "brightblack"))
   (base2      '("#070C14" "#070C14" "brightblack"))
   (base3      '("#060B12" "#060B12" "brightblack"))
   (base4      '("#06090F" "#06090F" "brightblack"))
   (base5      '("#05080D" "#05080D" "brightblack"))
   (base6      '("#04060A" "#04060A" "brightblack"))
   (base7      '("#030508" "#030508" "brightblack"))
   (base8      '("#020305" "#020305" "brightblack"))
   (fg         '("#92a1ba" "#92a1ba" "white"))        ; Universal foreground
   (fg-alt     '("#92a0b9" "#92a0b9" "brightwhite"))  ; Brighter foreground

   (grey       '("#AAAAAA" "#AAAAAA" "brightblack"))  ; Comments
   (red        '("#db9b9b" "#db9b9b" "red"))          ; Errors
   (orange     '("#ec9a34" "#ec9a34" "brightred"))    ; Numbers
   (yellow     '("#c4a500" "#c4a500" "yellow"))       ; Highlight
   (green      '("#88ad9c" "#88ad9c" "green"))        ; Strings
   (blue       '("#9dafc6" "#9dafc6" "brightblue"))   ; Functions
   (dark-blue  '("#79acd9" "#79acd9" "blue"))         ; Darker blue
   (teal       '("#88ad9c" "#88ad9c" "brightcyan"))   ; Types
   (magenta    '("#b9a3b7" "#b9a3b7" "magenta"))      ; Keywords
   (violet     '("#b9a3b7" "#b9a3b7" "brightmagenta")) ; Brighter purple
   (cyan       '("#74b9b4" "#74b9b4" "cyan"))         ; Constants
   (dark-cyan  '("#8faead" "#8faead" "cyan"))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   base0)
   (selection      '("#a8a8a8" "#a8a8a8"))   ; Universal selection
   (builtin        orange)
   (comments       (if doom-kera-brighter-comments grey grey))
   (doc-comments   (doom-lighten comments 0.15))
   (constants      cyan)
   (functions      blue)                     ; Functions (medium-light gray)
   (keywords       magenta)                  ; Keywords (lighter gray)
   (methods        blue)
   (operators      dark-blue)
   (type           teal)                     ; Types (medium gray)
   (strings        green)                    ; Strings (medium gray)
   (variables      '("#caa98f" "#caa98f" "brightwhite")) ; Variables (very light)
   (numbers        orange)
   (region         '("#a6a6a6" "#a6a6a6"))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; common
   ;; (common-accent   '("#E8E0D0" "yellow"  "yellow" ))
   ;; (common-bg       '("#1A1A1D" "black"   "black"  ))
   ;; (common-fg       '("#E8E8E8" "grey"    "grey"   ))
   ;; (common-ui       '("#9A9A9A" "grey"    "grey"   ))
   ;; (test            '("#D0D0D0" "white"   "white"  ))

   ;; syntax
   ;; (syntax-tag      '("#CFCFC8" "white"   "white"  ))
   ;; (syntax-func     '("#E8E0D0" "yellow"  "yellow" ))
   ;; (syntax-entity   '("#D0D0D0" "white"   "white"  ))
   ;; (syntax-string   '("#D0D8C8" "green"   "green"  ))
   ;; (syntax-regexp   '("#CFCFC8" "white"   "white"  ))
   ;; (syntax-markup   '("#E3D2C4" "red"     "red"    ))
   ;; (syntax-keyword  '("#E0D0C0" "orange"  "orange" ))
   ;; (syntax-special  '("#E8E0D0" "yellow"  "yellow" ))
   ;; (syntax-comment  '("#9A9A9A" "grey"    "grey"   ))
   ;; (syntax-constant '("#E6D5C0" "orange"  "orange" ))
   ;; (syntax-operator '("#D0D0D0" "white"   "white"  ))
   ;; (syntax-error    '("#E3D2C4" "red"     "red"    ))

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-kera-brighter-modeline)
   (-modeline-pad
    (when doom-kera-padded-modeline
      (if (integerp doom-kera-padded-modeline) doom-kera-padded-modeline 2)))

   (modeline-fg     fg-alt)
   (modeline-fg-alt comments)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      (doom-darken bg-alt 0.1)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#9ea5b1" :foreground "#151618")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.4))

   ((line-number &override) :foreground "#8b94a4" :background base0)
   ((line-number-current-line &override) :foreground fg :background bg-alt :bold t)

   (hl-line :background (doom-darken bg-alt 0.1))

   (region :foreground "#151618" :background region)

   (font-lock-comment-face
    :foreground comments
    :background (when doom-kera-comment-bg (doom-lighten bg 0.03)))

   ;; Search customization
   (isearch :background "#94a6c9" :foreground yellow :bold t)
   (lazy-highlight :background "#96a8ce" :foreground green)
   (completions-highlight-face :background "#8ba2cd" :foreground yellow)
   (vertico-current :background bg-alt :foreground yellow :bold t)
   (consult-file :foreground green)
   (marginalia-file-name :foreground blue)
   ((shadow &override) :foreground fg-alt)
   (completions-annotations :foreground comments)
   (marginalia-file-priv-dir :inherit 'shadow)
   (marginalia-file-priv-no :inherit 'shadow)
   (file-name-shadow
    :foreground base6
    :background (doom-darken bg 0.03)
    :italic nil)

   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground fg :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector            :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground orange)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'doom-kera-theme)

;;; doom-kera-theme.el ends here
