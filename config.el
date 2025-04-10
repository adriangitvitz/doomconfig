;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'window-setup-hook #'my/tool-bar-on-and-off)
(add-hook 'after-make-frame-functions (lambda (frame) (my/tool-bar-on-and-off)))

(setq rainbow-ansi-colors t
      rainbow-x-colors t)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq org-directory "~/org/")

(setq display-line-numbers-type t)

(setq doom-font (font-spec :family "MonoLisa" :size 19 :weight 'bold)
     doom-variable-pitch-font (font-spec :family "MonoLisa" :size 19 :weight 'bold))

(setq line-spacing 0.4)
(setq mac-allow-anti-aliasing t)

(setq doom-theme 'doom-kera)

(setq doom-themes-enable-bold t)     ; Maintain readability
(setq doom-themes-enable-italic nil)  ; Reduce eye strain
(setq doom-themes-padded-modeline t)  ; Better visual separation

(defun my/tool-bar-on-and-off ()
  (tool-bar-mode 1)
  (tool-bar-mode 0))

(setq doom-variable-pitch-ui t)    ; Enable variable pitch fonts
(setq display-line-numbers-type 'relative)  ; Reduces visual noise
;; (setq scroll-margin 5)                      ; Smoother scrolling
(setq scroll-conservatively 10001)         ; Less motion blur

(setq frame-background-mode 'dark)  ; Dark background mode

(setq evil-emacs-state-cursor  '("#B85C5C" box))  ; For normal mode
(setq evil-normal-state-cursor '("#B85C5C" box))  ; For emacs mode

;; (custom-theme-set-faces! 'doom-tomorrow-night
;;  '(default :background "#1A1A1D" :foreground "#E8E8E8" :weight bold) ;; Slightly cooler background
;;  '(region :background "#2D2A32" :weight bold)) ;; More purple-tinted selection

;; (custom-set-faces!
;;   '(default :foreground "#E8E8E8") ;; Whiter text to counteract yellow light
;;   '(line-number :foreground "#5B6268")
;;   '(line-number-current-line :foreground "#F8F8F8" :background "#353535") ;; More white/purple to combat yellow
;;   '(font-lock-comment-face :foreground "#A09890")  ;; Slightly grayer to remain visible
;;   '(font-lock-keyword-face :foreground "#D1B280" :weight bold) ;; Less yellow, more white/pink
;;   '(font-lock-string-face :foreground "#F2F1F6") ;; Less orange, more neutral/purple
;;   '(font-lock-constant-face :foreground "#F8F8F8") ;; More neutral gray
;;   '(font-lock-variable-name-face :foreground "#F8F8F8" :bold bold) ;; Cooler white
;;   '(font-lock-builtin-face :foreground "#D1B280" :bold bold) ;; Cooler white
;;   '(font-lock-function-name-face :foreground "#F8F8F8" :bold bold) ;; Slight blue tint but mostly white
;;   '(font-lock-preprocessor-face :foreground "#D1B280") ;; Muted blue-gray
;;   '(font-lock-type-face :foreground "#F2F1F6") ;; Less yellow
;;   '(font-lock-doc-face :foreground "#A8B0B8") ;; Documentation comments

;;   ;; UI blues
;;   '(highlight :background "#303848") ;; Deep blue-gray highlight
;;   '(isearch :background "#4A4A68" :foreground "#E8E8F0") ;; Purplish-blue search highlight
;;   '(lazy-highlight :background "#384048") ;; Secondary search matches

;;   ;; Info/help/documentation blues
;;   '(info-title-1 :foreground "#C0C8D8" :weight bold) ;; Blue-tinted headers
;;   '(info-title-2 :foreground "#B0B8C8" :weight bold)

;;   ;; Completion and popup blues
;;   '(company-tooltip-selection :background "#384050") ;; Selection in completion
;;   '(company-tooltip-common :foreground "#A0B0C8") ;; Common prefix in completion

;;   ;; Org mode faces
;;   '(org-document-info-keyword :foreground "#B0A8B0") ;; Slight purple tint
;;   '(org-document-title :foreground "#E0D8E0" :weight bold) ;; Cooler white
;;   '(org-headline-done :foreground "#B0A8B0")
;;   '(org-meta-line :foreground "#B0A8B0")
;;   '(org-block :background "#1A1A1D" :extend t) ;; Match main background
;;   '(org-block-begin-line :background "#1A1A1D" :foreground "#B0A8B0" :extend t)
;;   '(org-block-end-line :background "#1A1A1D" :foreground "#B0A8B0" :extend t)

;;   ;; UI elements with increased contrast
;;   '(mode-line :background "#242428" :foreground "#D0C8D0") ;; Cooler mode line
;;   '(mode-line-inactive :background "#1A1A1D")
;;   '(mode-line-highlight  :foreground "#E8E0E8")
;;   '(region :background "#383040" :extend t) ;; Much more distinct selection with purple tint
;;   '(hl-line :background "#202025") ;; Cooler line highlight
;;   '(tree-sitter-hl-face:operator :foreground "#F5F1E8") ;; Whiter
;;   '(shadow :foreground "#D8D0D8")

;;   ;; Link and reference colors
;;   '(link :foreground "#E8E4E2" :underline t) ;; Saturated blue-lavender with underline for visibility
;;   '(link-visited :foreground "#B0A8C8" :underline t) ;; Purple-blue visited links

;;   ;; Diff and version control blues
;;   '(diff-changed :background "#2A3040") ;; Changed lines in diffs
;;   '(magit-diff-context-highlight :background "#2A3040") ;; Changed context in magit
;;   '(magit-hash :foreground "#A0A8C0") ;; Commit hashes in magit

;;   '(sp-pair-overlay-face :background "#303848") ;; Smartparens overlay

;;   ;; Message and alert blues that need to remain visible
;;   '(compilation-info :foreground "#A0B8D0" :weight bold) ;; Compilation information
;;   '(success :foreground "#A0C0C0") ;; Success messages (blue-green tint)

;;   ;; LSP and IDE feature blues
;;   '(lsp-face-highlight-read :background "#384050") ;; LSP read highlights
;;   '(lsp-face-highlight-write :background "#403850") ;; LSP write highlights
;;   '(lsp-ui-doc-background :background "#252830") ;; Documentation popup background
;;   '(lsp-face-highlight-textual :foreground "#F4ECD8")

;;   ;; Additional elements for visibility
;;   '(show-paren-match :background "#3A4050" :foreground nil) ;; Matching parentheses
;;   '(vertical-border :foreground "#282830") ;; Cooler border
;;   '(markdown-header-face :foreground "#F0E8F0" :weight bold) ;; Whiter headers
;;   '(magit-section-highlight :background "#252530") ;; More distinct with purple tint
;;   '(diff-added-highlight :background "#283828") ;; More intense green for diffs
;;   '(diff-removed-highlight :background "#382828") ;; More intense red for diffs

;;   '(web-mode-keyword-face :foreground "#D1B280" :weight bold)
;;   '(web-mode-string-face  :foreground "#F0F5F1")
;;   '(web-mode-html-tag-face :foreground "#A0A8D0")
;;   '(web-mode-block-control-face :foreground "#C7A86D")
;;   '(web-mode-html-attr-value-face :foreground "#8CA5A8")
;;   '(rainbow-delimiters-depth-1-face :foreground "#F0E8F0")
;;   '(js2-function-call :foreground "#E8E4E2")
;;   '(js2-function-param :foreground "#D1B280")
;; )

;; (custom-theme-set-faces! 'doom-tomorrow-night
;;  '(default :background "#181818" :foreground "#E0E0E0" :weight bold)
;;  '(region :background "#181818" :weight bold))

;; (custom-set-faces!
;;   '(default :foreground "#E6E0D8")
;;   '(line-number :foreground "#5B6268")  ;; Keep this muted tone
;;   ;; '(line-number-current-line :foreground "#8A8D93")  ;; Less contrast from regular line numbers
;;   '(line-number-current-line :foreground "#B0A89F")
;;   '(font-lock-comment-face :foreground "#A89F8D")  ;; More muted than current green-yellow
;;   '(font-lock-keyword-face :foreground "#D0C0A8" :weight bold)  ;; Consistent color for all keywords
;;   '(font-lock-string-face :foreground "#C0B0A0")
;;   '(font-lock-constant-face :foreground "#B0A89F")  ;; For numbers and constants
;;   '(font-lock-variable-name-face :foreground "#C2B099" :bold bold)
;;   '(font-lock-builtin-face :foreground "#C2B099" :bold bold)
;;   '(font-lock-function-name-face :foreground "#E6D8C8" :bold bold)
;;   '(font-lock-type-face :foreground "#C2B099")  ;; For type annotations
;;   '(org-document-info-keyword :foreground "#B0A89F")  ;; More neutral
;;   '(org-document-title :foreground "#B0A89F")  ;; More neutral
;;   '(org-headline-done :foreground "#B0A89F")  ;; More neutral
;;   '(org-meta-line :foreground "#B0A89F")  ;; More neutral
;;   '(org-block :background "#181818" :extend t)  ;; Keep this
;;   '(org-block-begin-line :background "#181818" :foreground "#B0A89F" :extend t)  ;; Match other org elements
;;   '(org-block-end-line :background "#181818" :foreground "#B0A89F" :extend t)  ;; Match other org elements
;;   '(mode-line :background "#181818")  ;; Keep this
;;   '(mode-line-inactive :background "#181818")  ;; Keep this
;;   '(region :background "#343434" :extend t)
;;   '(tree-sitter-hl-face:operator :foreground "#D0C8C0")
;;   '(shadow :foreground "#D0C8C0")
;; )

(add-to-list 'load-path "~/Documents/Personal/emacsplugins/org-present")
(autoload 'org-present "org-present" nil t)

(add-hook 'org-present-mode-hook
        (lambda ()
        (org-present-small)
        (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
        (lambda ()
        (org-present-small)
        (org-remove-inline-images)))

(after! lsp-java
  (setq lsp-java-server-install-dir "/Users/adriannajera/.local/share/nvim/mason/packages/jdtls/"
        lsp-java-jdt-download-url nil  ; Disable auto-download
        lsp-java-configuration-runtimes [
          (:name "JavaSE-21"
           :path "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
           :default t)
        ]
        lsp-java-workspace-dir "~/java_workspace/"))

(setq lsp-java-vmargs
      ["-Xmx4G" "-XX:+UseG1GC"])

(after! js2-mode
  (custom-set-faces!
    '(js2-object-property :foreground "#C2B099")
    )
  )

;; Suggested changes
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.html$" . "html"))
  (add-hook 'html-mode-hook #'lsp!))

(after! magit
  (define-key magit-status-mode-map (kbd "s") 'magit-stage)
  (define-key magit-status-mode-map (kbd "S") 'magit-stage-all))

(use-package! web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
)

(add-hook 'web-mode-hook #'emmet-mode) ; For HTML tag expansion

(use-package! dumb-jump
  :config (setq dumb-jump-selector 'ivy))

(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/adriannajera/Documents/Notes/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (after! lsp-mode
;;   (custom-set-faces!
;;     '(lsp-face-semhl-operator :foreground "#FFD700")
;;     ))
