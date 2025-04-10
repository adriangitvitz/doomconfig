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

;; config.el
(setq doom-theme 'doom-tomorrow-night)
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

(custom-theme-set-faces! 'doom-tomorrow-night
 '(default :background "#181818" :foreground "#E0E0E0" :weight bold)
 '(region :background "#181818" :weight bold))

(custom-set-faces!
  '(default :foreground "#E6E0D8")
  '(line-number :foreground "#5B6268")  ;; Keep this muted tone
  ;; '(line-number-current-line :foreground "#8A8D93")  ;; Less contrast from regular line numbers
  '(line-number-current-line :foreground "#B0A89F")
  '(font-lock-comment-face :foreground "#A89F8D")  ;; More muted than current green-yellow
  '(font-lock-keyword-face :foreground "#D0C0A8" :weight bold)  ;; Consistent color for all keywords
  '(font-lock-string-face :foreground "#C0B0A0")
  '(font-lock-constant-face :foreground "#B0A89F")  ;; For numbers and constants
  '(font-lock-variable-name-face :foreground "#C2B099" :bold bold)
  '(font-lock-builtin-face :foreground "#C2B099" :bold bold)
  '(font-lock-function-name-face :foreground "#E6D8C8" :bold bold)
  '(font-lock-type-face :foreground "#C2B099")  ;; For type annotations
  '(org-document-info-keyword :foreground "#B0A89F")  ;; More neutral
  '(org-document-title :foreground "#B0A89F")  ;; More neutral
  '(org-headline-done :foreground "#B0A89F")  ;; More neutral
  '(org-meta-line :foreground "#B0A89F")  ;; More neutral
  '(org-block :background "#181818" :extend t)  ;; Keep this
  '(org-block-begin-line :background "#181818" :foreground "#B0A89F" :extend t)  ;; Match other org elements
  '(org-block-end-line :background "#181818" :foreground "#B0A89F" :extend t)  ;; Match other org elements
  '(mode-line :background "#181818")  ;; Keep this
  '(mode-line-inactive :background "#181818")  ;; Keep this
  '(region :background "#343434" :extend t)
  '(tree-sitter-hl-face:operator :foreground "#D0C8C0")
  '(shadow :foreground "#D0C8C0")
)

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
