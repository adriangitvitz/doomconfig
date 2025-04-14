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
  ((bg         '("#0D1017" nil nil))          ; L=8
   (bg-alt     '("#131721" nil nil))          ; L=12
   (base0      '("#171C26" "black" "black"))  ; L=15
   (base1      '("#1D2332" "#1D2332" "brightblack"))
   (base2      '("#242B3D" "#242B3D" "brightblack"))
   (base3      '("#2D354A" "#2D354A" "brightblack"))
   (base4      '("#3D4759" "#3D4759" "brightblack"))
   (base5      '("#566178" "#566178" "brightblack"))
   (base6      '("#6C7A9C" "#6C7A9C" "brightblack"))
   (base7      '("#8A93A8" "#8A93A8" "brightblack"))
   (base8      '("#BFBDB6" "#BFBDB6" "white"))  ; Ayu foreground[3]
   (fg         '("#BFBDB6" "#BFBDB6" "white"))  ; Text
   (fg-alt     '("#A8A8A8" "#A8A8A8" "brightwhite"))

   (grey       '("#9A9A9A" "#9A9A9A" "brightblack"  ))
   (red        '("#E3D2C4" "#E3D2C4" "red"          ))
   (orange     '("#E6D5C0" "#E6D5C0" "brightred"    ))
   (yellow     '("#D8C8A8" "#D8C8A8" "yellow"))
   (green      '("#D0D8C8" "#D0D8C8" "green"        ))
   (blue       '("#D0D0D0" "#D0D0D0" "brightblue"   ))
   (dark-blue  '("#A8A8A8" "#A8A8A8" "blue"         ))
   (teal       '("#CFCFC8" "#CFCFC8" "brightcyan"   ))
   (magenta    '("#E0D0C0" "#E0D0C0" "magenta"      ))
   (violet     '("#CACAC8" "#CACAC8" "brightmagenta"))
   (cyan       '("#8AA8B8" "#8AA8B8" "cyan"))       ; Soft cyan
   (dark-cyan  '("#6C8A9C" "#6C8A9C" "cyan"))

   ;; face categories -- required for all themes
   (highlight      teal)                     ; Subtle highlight
   (vertical-bar   base0)
   (selection      '("#2A3040" "#2A3040"))   ; Low-contrast selection
   (builtin        orange)
   (comments       (doom-lighten grey 0.1))  ; Controlled brightness
   (doc-comments   (doom-lighten grey 0.25))
   (constants      red)
   (functions      blue)                     ; Muted blue functions
   (keywords       teal)                     ; Ayu-inspired teal
   (methods        blue)
   (operators      dark-blue)
   (type           orange)
   (strings        green)                    ; Pastel green strings
   (variables      (doom-lighten blue 0.3))
   (numbers        orange)
   (region         '("#2A3040" "#2A3040"))   ; Unified region color
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; common
   (common-accent   '("#E8E0D0" "yellow"  "yellow" ))
   (common-bg       '("#1A1A1D" "black"   "black"  ))
   (common-fg       '("#E8E8E8" "grey"    "grey"   ))
   (common-ui       '("#9A9A9A" "grey"    "grey"   ))
   (test            '("#D0D0D0" "white"   "white"  ))

   ;; syntax
   (syntax-tag      '("#CFCFC8" "white"   "white"  ))
   (syntax-func     '("#E8E0D0" "yellow"  "yellow" ))
   (syntax-entity   '("#D0D0D0" "white"   "white"  ))
   (syntax-string   '("#D0D8C8" "green"   "green"  ))
   (syntax-regexp   '("#CFCFC8" "white"   "white"  ))
   (syntax-markup   '("#E3D2C4" "red"     "red"    ))
   (syntax-keyword  '("#E0D0C0" "orange"  "orange" ))
   (syntax-special  '("#E8E0D0" "yellow"  "yellow" ))
   (syntax-comment  '("#9A9A9A" "grey"    "grey"   ))
   (syntax-constant '("#E6D5C0" "orange"  "orange" ))
   (syntax-operator '("#D0D0D0" "white"   "white"  ))
   (syntax-error    '("#E3D2C4" "red"     "red"    ))

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-kera-brighter-modeline)
   (-modeline-pad
    (when doom-kera-padded-modeline
      (if (integerp doom-kera-padded-modeline) doom-kera-padded-modeline 2)))

   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (modeline-bg
    (if -modeline-bright
        (doom-darken teal 0.45)
      (doom-darken (car bg-alt) 0.15)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken teal 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.2))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.4))

   ((line-number &override) :foreground base6 :background bg)
   ((line-number-current-line &override) :foreground fg :background bg-alt :bold t)

   (hl-line :background (doom-darken bg-alt 0.1))

   (font-lock-comment-face
    :foreground comments
    :background (when doom-kera-comment-bg (doom-lighten bg 0.03)))

   ;; Search customization
   (isearch :background "#2A3040" :foreground teal :bold t)
   (lazy-highlight :background "#1F2530" :foreground green)
   (completions-highlight-face :background "#242428" :foreground "#E8E0D0")
   (vertico-current :background bg-alt :foreground teal :bold t)
   (consult-file :foreground "#D0D8C8")
   (marginalia-file-name :foreground "#D0D0D0")
   ((shadow &override) :foreground (doom-lighten base7 0.1))
   (completions-annotations :foreground comments)
   (marginalia-file-priv-dir :inherit 'shadow)
   (marginalia-file-priv-no :inherit 'shadow)
   (file-name-shadow
    :foreground (doom-lighten base7 0.1)
    :background (doom-darken bg 0.05)
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
