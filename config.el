;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'window-setup-hook #'my/tool-bar-on-and-off)
(add-hook 'after-make-frame-functions (lambda (frame) (my/tool-bar-on-and-off)))

(defun splash ()
  (let* ((banner '("█████▇▆▅▄▃▁▁▁▁ ▁▁    ▃▄▄▄▄▄▄▄▄▃▁▁▁▁    ▂▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▂▂▃▄▄▄▄▄▄▄▄▄▄▂▁▁▁▁▁▁▁▁▁▁▁▂▃▄▅▅▆██"
                   "████▇███▇▅▂▁▁▁▁▁▁▁▁  ▂▃▄▄▄▄▃▃▂▁▁▁▁     ▁▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▃▁▂▄▄▄▄▄▄▄▃▁   ▃▄▃▃▃▃▃▃▃▃  ▁▁▁▁▁▁▁▁▂▄▅▅▅▅▆▇██"
                   "████▇▇▇▇▅▂ ▁ ▁▁▁▁▁▁▁  ▁▂▃▃▃▃▂▁▁▁▁▁ ▂▃  ▂▃▃▃▃▃▃▃▃▃▃▃▃▃▃▂▁  ▁▃▃▃▃▃▃▂▁   ▁▂▃▃▃▂▁ ▁▁▁ ▁▁▁▁▁▁▃▅▅▅▅▅▅▆▇███"
                   "█████▇▆▄▅▅▄▂▁▁▁▁▁▁▁  ▁  ▂▂▂▂▁▁▁▁▁▂▄▅▃▁ ▂▂▂▂▂▂▃▃▃▃▃▃▃▃▂▁▁▁  ▂▂▂▂▂▂▁▂▃▁   ▂▂▂     ▁▁▁▁▁▂▄▅▅▅▅▅▅▆▆▆▇███"
                   "████▇██▇▆▄▅▄▅▄▂▁▁▁▁▂▃▂▁ ▁▂▁▁▁▁▁▁▃▅▅▅▂▁  ▃▂▂▂▂▁▂▂▂▂▂▂▂▁▁▁    ▂▂▂▂▁▁▃▅▃▁▁  ▁▁▁▁  ▁▁▁▁▃▄▆▆▆▆▆▆▆▅▆▇▇▇▇██"
                   "██▇▇▇▆▆▅▄▅▄▄▄▅▄▂▂▂▃▄▄▃▂▁ ▁▁▁▁▁▂▄▅▅▃▃▁▁▃▄▅▂▂▂▂▂▁▁▁▁▁▂▁▁▁     ▁▁▁▁ ▁▄▄▄▂▂▃▂▂ ▁▁▁▁▁▂▃▅▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆██"
                   "██▇▇▇▇▆▃▂▂▄▅▅▄▄▃▃▃▃▃▃▄▃▂▁  ▁▂▃▄▅▅▅▄▂▁▁▄▅▄▂▂▁▁▁▁▁▁▁▁▁▁▁▁         ▁▂▄▄▄▂▁▃▂▂▁▁▁▁▁▂▅▆▆▆▆▆▆▆▆▅▆▆▆▅▅▅▆███"
                   "██▇▇▇▇██▆▅▃▄▅▄▄▄▄▃▃▄▄▄▃▁▂▂▁▁▄▄▄▅▅▃▄▄▁▁▃▅▃▂▃▁▁▁▁▁▁▁▁▁▁▁▁▁▁      ▂▂▃▄▄▄▂▂▂▃▂▂▁  ▁▅▆▆▆▅▅▅▆▅▅▅▅▅▅▅▅▆▇███"
                   "███▇▇▆▇██▇▇▇▇▇▅▄▄▄▄▅▄▄▂▁▁▁▃▃▃▄▅▅▅▅▃▄▂▁▂▅▃▂▃▁▁      ▂▂▁ ▁▁▁    ▂▅▄▂▃▄▃▂▁▂▂▂▁▂▁ ▂▆▆▆▅▅▅▅▅▅▅▅▅▅▆▆▇█████"
                   "████▇▇▇▇████▇▆▄▅▄▄▄▅▄▄▃▁▃▃▃▃▄▄▅▅▅▅▄▂▂▁▁▄▄▄▂▂▁▁   ▁▁▃▂▁▁▁ ▁ ▁▁▃▄▄▄▂▂▄▃▁▁▁▂▁▁▁▂▂▄▆▅▅▅▅▅▅▅▅▅▆▆▆▆▆▇▆▆███"
                   "███████▇███▇▄▂▅▄▄▄▄▅▅▃▃▄▄▄▄▄▄▅▅▅▅▅▅▂▁▁▁▄▃▃▃▁▁▁ ▁▁▃▃▃▃ ▁▁▁▁▁▁▁▄▄▄▂▃▃▂▃▃▃▁▁▂▂▁ ▁▅▅▅▅▅▅▆▆▆▆▆▅▄▄▅▅▃▃▇▇██"
                   "█████████▇▆▅▅▃▂▅▄▅▄▄▄▄▃▃▃▃▃▄▄▄▅▅▅▅▅▃▁▁▁▂▃▂▃▂▁▁▁▂▃▃▃▃▂ ▂▂▂▁▁▁▂▄▄▄▂▁▂▃▂▁▃▃▂▁▂▃▂▄▅▅▅▆▆▆▅▄▃▃▆▅▅▅▃▂▃▆▇▇██"
                   "█████▇█▇▆▅▂▁▂▅▄▅▅▄▄▅▄▄▄▄▄▄▄▄▄▅▅▅▅▅▅▂▁▁▁▂▄▂▃▃▁▂▂▂▂▂▂▃▂  ▁▂▂▁▂▃▄▄▄▃▁▁▂▃▂▂▃▄▄▄▅▅▆▅▆▆▆▆▆▅▅▅▃▄▆▅▃▆▇██████"
                   "██▇▇███▇▆▄▄▄▂▂▅▄▄▅▃▄▅▄▄▄▄▃▄▄▄▄▄▄▅▅▅▃▁▃▁▂▄▁▁▄▂▁▂▂▁▂▂▂▂▂▁  ▁▂▃▄▄▄▄▃▂▂▂▃▄▅▅▅▅▅▅▆▆▆▆▅▅▅▆▆▆▆▄▃▃▆▆▆▇███▇██"
                   "██▇▇▇████▇▄▅▅▄▄▅▄▄▄▂▄▅▄▄▅▄▃▃▃▄▄▄▄▅▅▄▁▂▂▂▄▂▁▃▃▂▂▂▁▂▂▂▁▁▁▁▁▂▄▄▄▄▄▄▄▂▁▂▄▆▅▅▅▅▆▆▆▆▆▆▆▄▄▆▆▆▆▅▄▄▅▄▂▂▆▆▇▇██"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'splash)

(setq rainbow-ansi-colors t
      rainbow-x-colors t)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq org-directory "~/org/")

(setq display-line-numbers-type nil)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq doom-font (font-spec :family "Cascadia Code" :size 20 :weight 'bold)
      doom-variable-pitch-font (font-spec :family "Cascadia Code" :size 20 :weight 'bold))

(setq line-spacing 0.4)
(setq mac-allow-anti-aliasing t)

;; (setq doom-theme 'doom-kera)
(setq doom-theme 'doom-monochro)
;; (setq doom-theme 'doom-nord-aurora)

(setq doom-themes-enable-bold t)     ; Maintain readability
(setq doom-themes-enable-italic nil)  ; Reduce eye strain
(setq doom-themes-padded-modeline t)  ; Better visual separation

(defun my/tool-bar-on-and-off ()
  (tool-bar-mode 1)
  (tool-bar-mode 0))

(setq doom-variable-pitch-ui t)    ; Enable variable pitch fonts
(setq display-line-numbers-type 'relative)  ; Reduces visual noise
(setq scroll-conservatively 10001)         ; Less motion blur

(setq frame-background-mode 'dark)  ; Dark background mode

(setq evil-emacs-state-cursor  '("#d09393" box))  ; For normal mode
(setq evil-normal-state-cursor '("#d09393" box))  ; For emacs mode

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

(after! apheleia
  ;; Define the formatter
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("/Users/adriannajera/.local/share/nvim/mason/packages/google-java-format/google-java-format" "-"))

  (setf (alist-get 'java-mode apheleia-mode-alist)
        '(google-java-format)))

(add-hook 'java-mode-hook #'apheleia-mode)

(after! format
  (setq +format-on-save-enabled-modes
        '(not emacs-lisp-mode sql-mode tex-mode latex-mode java-mode)))


;; (after! lsp-java
;;   (setq lsp-java-server-install-dir "/Users/adriannajera/.local/share/nvim/mason/packages/jdtls/"
;;         lsp-java-jdt-download-url nil  ; Disable auto-download
;;         lsp-java-vmargs
;;         [
;;             "-Xmx8G"  ;; For large projects (4G < 8G < 12G)
;;             "-XX:+UseG1GC"
;;             "-XX:+PerfDisableSharedMem"  ;; Critical for Linux
;;             "-XX:+AlwaysPreTouch"
;;             "-XX:ReservedCodeCacheSize=1G"
;;             "-XX:+UseStringDeduplication"
;;             "-XX:MaxInlineLevel=15"
;;             "-Djava.net.preferIPv4Stack=true"
;;         ]
;;         lsp-java-configuration-runtimes [
;;                                          (:name "JavaSE-21"
;;                                           :path "/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
;;                                           :default t)
;;                                          ]
;;         lsp-java-workspace-dir "/Users/adriannajera/java_workspace/"))

;; (setq lsp-diagnostics-update-in-delay 0.5  ;; Default: 0.1
;;       lsp-idle-delay 1.0)  ;; Default: 0.5
;;
;; (setq lsp-java-vmargs
;;       ["-Xmx4G" "-XX:+UseG1GC"])

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

;; (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)


(setq org-directory "~/Documents/Notes/roam"
      org-roam-directory org-directory
      org-roam-db-location (file-name-concat org-directory ".org-roam.db")
      org-roam-dailies-directory "journal/"
      org-archive-location (file-name-concat org-directory ".archive/%s::")
      org-agenda-files (list org-directory)
      org-log-done-with-time nil
      org-habit-show-habits-only-for-today nil)

(after! org
  (add-to-list 'org-modules 'org-habit)
  (setq org-startup-folded 'show2levels
        org-ellipsis " [...] "
        org-capture-templates
        '(("t" "todo" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("d" "deadline" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "schedule" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("c" "check out later" entry (file+headline "todo.org" "Check out later")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("l" "ledger" plain (file "ledger/personal.gpg")
           "%(+beancount/clone-transaction)"))))

(after! org-agenda
  (setq org-agenda-todo-list-sublevels nil
        org-agenda-compact-blocks t
        org-agenda-sorting-strategy
        '((agenda time-up category-keep habit-up priority-down)
          (todo priority-down category-keep) (tags priority-down category-keep)
          (search category-keep))))

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("r" "thought" plain
           ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "topic" plain
           ,(format "#+title: ${title}\n%%[%s/template/topic.org]" org-roam-directory)
           :target (file "topic/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain
           ,(format "#+title: ${title}\n%%[%s/template/contact.org]" org-roam-directory)
           :target (file "contact/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("i" "invoice" plain
           ,(format "#+title: %%<%%Y%%m%%d>-${title}\n%%[%s/template/invoice.org]" org-roam-directory)
           :target (file "invoice/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("f" "ref" plain
           ,(format "#+title: ${title}\n%%[%s/template/ref.org]" org-roam-directory)
           :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org.gpg")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        `(("d" "default" plain ""
           :target (file+head "%<%Y-%m-%d>.org" ,(format "%%[%s/template/journal.org]" org-roam-directory))))))

(after! org-tree-slide
  ;; I use g{h,j,k} to traverse headings and TAB to toggle their visibility, and
  ;; leave C-left/C-right to .  I'll do a lot of movement because my
  ;; presentations tend not to be very linear.
  (setq org-tree-slide-skip-outline-level 2))

(after! org-roam
  ;; Offer completion for #tags and @areas separately from notes.
  (add-to-list 'org-roam-completion-functions #'org-roam-complete-tag-at-point)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)

  ;; Make the backlinks buffer easier to peruse by folding leaves by default.
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; List dailies and zettels separately in the backlinks buffer.
  (advice-add #'org-roam-backlinks-section :override #'org-roam-grouped-backlinks-section)

  ;; Open in focused buffer, despite popups
  (advice-add #'org-roam-node-visit :around #'+popup-save-a)

  ;; Make sure tags in vertico are sorted by insertion order, instead of
  ;; arbitrarily (due to the use of group_concat in the underlying SQL query).
  (advice-add #'org-roam-node-list :filter-return #'org-roam-restore-insertion-order-for-tags-a)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'org-roam-add-preamble-a))

;; Hide the menu for as minimalistic a startup screen as possible.
;; (setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; (after! corfu
;;   (setq corfu-auto nil))

(setq doom-modeline-modal nil
      doom-modeline-check-simple-format t)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq evil-ex-substitute-global t)

;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K

;; Bookmarks
(map! :leader
      (:prefix ("b". "buffer"))
      :desc "List bookmarks" "L" #'list-bookmarks)

;; Dired
(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
              (:map dired-mode-map
               :desc "Peep-dired image previews" "d p" #'peep-dired
               :desc "Dired view file"           "d v" #'dired-view-file)))

;; Markdown
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

;; Move lines up or down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-j") 'move-line-up)
(global-set-key (kbd "s-k") 'move-line-down)

(require 'ansi-color)

(defvar-local glow-temp-file nil
  "Temporary file for Glow markdown preview.")

(defvar-local glow-preview-buffer nil
  "Buffer used for Glow markdown preview.")

(defvar-local glow-update-timer nil
  "Timer for delayed preview updates.")

(defgroup glow-preview nil
  "Options for Glow Markdown previews."
  :group 'markdown)

(defcustom glow-style "dark"
  "Style to use for Glow preview."
  :type 'string
  :group 'glow-preview)

(defcustom glow-width 80
  "Width in columns for the Glow preview."
  :type 'integer
  :group 'glow-preview)

;; Define customization variables
(defgroup glow-preview nil
  "Live preview of markdown using Glow."
  :group 'markdown)

(defcustom glow-style "auto"
  "Style to use for Glow markdown rendering.
Can be \"auto\", \"dark\", \"light\" or a path to a JSON theme file."
  :type 'string
  :group 'glow-preview)

;; Ensure xterm-color is available for better color support
(use-package! xterm-color
  :defer t)

;; Glow minor mode
(setq glow-style "notty")

(define-minor-mode glow-live-preview-mode
  "Toggle live preview of markdown using Glow."
  :lighter " Glow"
  :global nil
  (if glow-live-preview-mode
      (progn
        ;; Ensure Glow is installed
        (unless (executable-find "glow")
          (user-error "Glow executable not found. Please install glow first"))

        (setq glow-temp-file (make-temp-file "glow-markdown-" nil ".md"))
        (setq glow-preview-buffer (get-buffer-create "*Glow Live Preview*"))
        (setq glow-update-timer nil)

        (with-current-buffer glow-preview-buffer
          (read-only-mode 1)
          (special-mode)
          (when (fboundp 'xterm-color-unfontify-region)
            (font-lock-mode -1))
          ;; Set up buffer-local face remapping to improve ANSI color display
          (face-remap-add-relative 'default :background (doom-color 'bg))
          (setq-local ansi-color-context-region nil)
          (unless (or (bound-and-true-p xterm-color-mode)
                      (bound-and-true-p ansi-color-mode))
            (when (fboundp 'xterm-color-mode)
              (xterm-color-mode))))

        (glow-update-preview)
        (display-buffer glow-preview-buffer
                        '(display-buffer-in-side-window
                          (side . right)
                          (window-width . 0.5)))

        (add-hook 'after-change-functions #'glow-schedule-update nil t)
        (add-hook 'kill-buffer-hook #'glow-cleanup nil t))

    (when glow-update-timer
      (cancel-timer glow-update-timer)
      (setq glow-update-timer nil))
    (remove-hook 'after-change-functions #'glow-schedule-update t)
    (remove-hook 'kill-buffer-hook #'glow-cleanup t)
    (glow-cleanup)))

(defun glow-schedule-update (&rest _)
  "Schedule an update of the Glow preview after a small delay."
  (when glow-update-timer
    (cancel-timer glow-update-timer))
  (setq glow-update-timer
        (run-with-idle-timer 0.5 nil #'glow-update-preview)))

(defun glow-update-preview ()
  "Update the Glow preview buffer."
  (when (and glow-temp-file glow-preview-buffer (buffer-live-p glow-preview-buffer))
    (let ((temp-file glow-temp-file)
          (preview-buffer glow-preview-buffer))
      (write-region (point-min) (point-max) temp-file nil 'no-message)
      (with-current-buffer preview-buffer
        (let ((inhibit-read-only t)
              (orig-point (point)))
          (erase-buffer)
          (let ((process-environment (append '("TERM=xterm-256color"
                                               "COLORTERM=truecolor"
                                               "FORCE_COLOR=3"
                                               "NO_COLOR=" ; Unset NO_COLOR
                                               "CLICOLOR=1"
                                               "CLICOLOR_FORCE=1")
                                             process-environment)))
            (let ((exit-code (call-process "glow" nil t nil
                                           "-s" glow-style
                                           "--width" (number-to-string (window-width))
                                           temp-file)))
              (if (= 0 exit-code)
                  (progn
                    (goto-char (point-min))
                    (ansi-color-apply-on-region (point-min) (point-max))
                    (when (fboundp 'xterm-color-colorize-buffer)
                      (xterm-color-colorize-buffer))
                    ;; Restore position or go to the beginning
                    (if (< orig-point (point-max))
                        (goto-char orig-point)
                      (goto-char (point-min))))
                (insert "Error running glow. Make sure it's installed correctly.")))))))))

(defun glow-cleanup ()
  "Clean up resources used by Glow preview."
  (when glow-update-timer
    (cancel-timer glow-update-timer)
    (setq glow-update-timer nil))

  (when glow-temp-file
    (let ((temp-file glow-temp-file))
      (when (file-exists-p temp-file)
        (delete-file temp-file))))

  (when (and glow-preview-buffer (buffer-live-p glow-preview-buffer))
    (kill-buffer glow-preview-buffer)))

;; Doom Emacs Glow keybinding
(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :desc "Toggle Glow Live Preview" "p G" #'glow-live-preview-mode))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(load! "leetcode-mode.el")

(use-package! leetcode-mode
  :commands (leetcode-search leetcode-random)
  :init
  (setq leetcode-lang       "cpp"
        leetcode-api-url    "https://leetcode.com/graphql"
        leetcode-buffer-name "*LeetCode*")
  :config
  ;; optional: enable the global keymap by default
  (leetcode-mode +1))

(after! org
  (plist-put org-format-latex-options :scale 1.80))

(load! "gleam-ts-mode")

(use-package! gleam-ts-mode
  :mode (rx ".gleam" eos))

;; Define language sources
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go" "v0.19.1")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (gomod . ("https://github.com/camdencheek/tree-sitter-gomod.git"))
        ))

;; Install grammars
(dolist (language (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p language)
    (treesit-install-language-grammar language)))

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))
