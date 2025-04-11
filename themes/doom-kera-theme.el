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
  ((bg         '("#1f2430" nil       nil            ))
   (bg-alt     '("#252b38" nil       nil            ))
   (base0      '("#171c26" "black"   "black"        ))
   (base1      '("#1a202c" "#1a202c" "brightblack"  ))
   (base2      '("#232a38" "#232a38" "brightblack"  ))
   (base3      '("#2d3747" "#2d3747" "brightblack"  ))
   (base4      '("#3d4759" "#3d4759" "brightblack"  ))
   (base5      '("#606a7e" "#606a7e" "brightblack"  ))
   (base6      '("#8a93a8" "#8a93a8" "brightblack"  ))
   (base7      '("#c0c7d5" "#c0c7d5" "brightblack"  ))
   (base8      '("#ebeef5" "#ebeef5" "white"        ))
   (fg         '("#d8dee9" "#d8dee9" "white"        ))
   (fg-alt     '("#c6cdd8" "#c6cdd8" "brightwhite"  ))

   (grey       '("#9A9A9A" "#9A9A9A" "brightblack"  ))
   (red        '("#E3D2C4" "#E3D2C4" "red"          ))
   (orange     '("#E6D5C0" "#E6D5C0" "brightred"    ))
   (yellow     '("#E8E0D0" "#E8E0D0" "yellow"       ))
   (green      '("#D0D8C8" "#D0D8C8" "green"        ))
   (blue       '("#D0D0D0" "#D0D0D0" "brightblue"   ))
   (dark-blue  '("#A8A8A8" "#A8A8A8" "blue"         ))
   (teal       '("#CFCFC8" "#CFCFC8" "brightcyan"   ))
   (magenta    '("#E0D0C0" "#E0D0C0" "magenta"      ))
   (violet     '("#CACAC8" "#CACAC8" "brightmagenta"))
   (cyan       '("#C8C8C0" "#C8C8C0" "cyan"         ))
   (dark-cyan  '("#A8A8A0" "#A8A8A0" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   base0)
   (selection      bg-alt)
   (builtin        orange)
   (comments       (doom-lighten grey 0.1))
   (doc-comments   (doom-lighten grey 0.25))
   (constants      red)
   (functions      yellow)
   (keywords       teal)
   (methods        yellow)
   (operators      blue)
   (type           (doom-lighten orange 0.1))
   (strings        green)
   (variables      (doom-lighten blue 0.4))
   (numbers        orange)
   (region         (doom-lighten selection 0.15))
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
      (if (integerp doom-kera-padded-modeline) doom-kera-padded-modeline 4)))

   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (modeline-bg
    (if -modeline-bright
        (doom-darken yellow 0.475)
      (doom-darken (car bg-alt) 0.15)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken yellow 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base6 :background bg)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-kera-comment-bg (doom-lighten bg 0.05)))

   ;; Search customization
   (isearch :background "#3D3D40" :foreground "#FFFFFF")
   (lazy-highlight :background "#323235")
   (completions-highlight-face :background "#242428" :foreground "#E8E0D0")
   (vertico-current :background "#242428" :foreground "#E8E0D0" :bold t)
   (consult-file :foreground "#D0D8C8")
   (marginalia-file-name :foreground "#D0D0D0")
   ((shadow &override) :foreground (doom-lighten base7 0.1))
   (completions-annotations :inherit 'shadow)
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
