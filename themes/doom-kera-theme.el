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
  "A custom theme for keratoconus"

  ;; name        default   256       16
  ((bg         '("#1A1A1D" nil       nil            ))
   (bg-alt     '("#242428" nil       nil            ))
   (base0      '("#161619" "black"   "black"        ))
   (base1      '("#1E1E22" "#1E1E22" "brightblack"  ))
   (base2      '("#25252A" "#25252A" "brightblack"  ))
   (base3      '("#313137" "#313137" "brightblack"  ))
   (base4      '("#45454D" "#45454D" "brightblack"  ))
   (base5      '("#666670" "#666670" "brightblack"  ))
   (base6      '("#9696A0" "#9696A0" "brightblack"  ))
   (base7      '("#D5D5DD" "#D5D5DD" "brightblack"  ))
   (base8      '("#E8E8F0" "#E8E8F0" "white"        ))
   (fg         '("#E8E8E8" "#E8E8E8" "white"        ))
   (fg-alt     '("#A69E93" "#A69E93" "brightwhite"  ))

   (grey       '("#8A8B8A" "#8A8A8A" "brightblack"  ))
   (red        '("#E6B3B3" "#E6B3B3" "red"          ))
   (orange     '("#E6C3A7" "#E6C3A7" "brightred"    ))
   (yellow     '("#E6D5B3" "#E6D5B3" "yellow"       ))
   (green      '("#C5D1B3" "#C5D1B3" "green"        ))
   (blue       '("#B3C7D9" "#B3C7D9" "brightblue"   ))
   (dark-blue  '("#7F9DB3" "#7F9DB3" "blue"         ))
   (teal       '("#B3D1CD" "#B3D1CD" "brightcyan"   ))
   (magenta    '("#D1C3D4" "#D1C3D4" "magenta"      ))
   (violet     '("#C4B3CC" "#C4B3CC" "brightmagenta"))
   (cyan       '("#B3D4D1" "#B3D4D1" "cyan"         ))
   (dark-cyan  '("#8AB3B0" "#8AB3B0" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base0)
   (selection      bg-alt)
   (builtin        magenta)
   (comments       orange) ;; TODO: See if could be later change
   (doc-comments   (doom-lighten comments 0.15))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         (doom-lighten selection 0.15))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; common
   (common-accent   '("#E6D5B3" "orange"  "orange" )) ;; Softer accent
   (common-bg       '("#1A1A1D" "black"   "black"  )) ;; Your background
   (common-fg       '("#E8E8E8" "grey"    "grey"   )) ;; Brighter foreground
   (common-ui       '("#8A8B8A" "grey"    "grey"   )) ;; Adjusted UI elements
   (test            '("#B3C7D9" "grey"    "grey"   )) ;; Softer blue

   ;; syntax
   (syntax-tag      '("#B3D4D1" "cyan"    "blue"   )) ;; Softer cyan
   (syntax-func     '("#E6D5B3" "yellow"  "yellow" )) ;; Warm yellow
   (syntax-entity   '("#B3C7D9" "blue"    "blue"   )) ;; Soft blue
   (syntax-string   '("#C5D1B3" "green"   "green"  )) ;; Sage green
   (syntax-regexp   '("#B3D1CD" "teal"    "green"  )) ;; Soft teal
   (syntax-markup   '("#E6B3B3" "red"     "red"    )) ;; Soft red
   (syntax-keyword  '("#E6C3A7" "orange"  "orange" )) ;; Warm orange
   (syntax-special  '("#E6D5B3" "yellow"  "yellow" )) ;; Warm yellow
   (syntax-comment  '("#8A8B8A" "grey"    "grey"   )) ;; Brighter comments
   (syntax-constant '("#D1C3D4" "magenta" "purple" )) ;; Soft magenta
   (syntax-operator '("#E6C3A7" "orange"  "orange" )) ;; Warm orange
   (syntax-error    '("#E6B3B3" "red"     "red"    )) ;; Soft red

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
        (doom-darken blue 0.475)
      (doom-darken (car bg-alt) 0.15)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base6 :background "#353535")
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-kera-comment-bg (doom-lighten bg 0.05)))

   ;; Search customization
   (isearch :background "#4A4A68" :foreground "#E8E8F0") ;; Purplish-blue search highlight
   (lazy-highlight :background "#384048") ;; Secondary search matches
   (completions-highlight-face :background "#242428" :foreground "#B3D4D1")
   (vertico-current :background "#242428" :foreground "#B3D4D1" :bold t)
   (consult-file :foreground "#B3D4D1")
   (marginalia-file-name :foreground "#B3C7D9")
   ((shadow &override) :foreground (doom-lighten base7 0.1))
   (completions-annotations :inherit 'shadow)
   (marginalia-file-priv-dir :inherit 'shadow)
   (marginalia-file-priv-no :inherit 'shadow)
   (file-name-shadow
        :foreground (doom-lighten base7 0.1)    ;; Using theme color variables
        :background (doom-darken bg 0.05)       ;; Optional background tint
        :italic nil)                            ;; Optional style settings

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
   (markdown-header-face :inherit 'bold :foreground red)
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
