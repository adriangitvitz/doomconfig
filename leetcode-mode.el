;;; leetcode-mode.el -*- lexical-binding: t; -*-

(require 'json)
(require 'shr)
(require 'cl-lib)
(require 'request)
;; Enable DEBUG
;; (setq request-log-level    'debug
;;       request-message-level 'debug)

(defgroup leetcode-mode nil
  "LeetCode integration for Doom Emacs."
  :group 'tools
  :prefix "leetcode-")

(defcustom leetcode-api-url "https://leetcode.com/graphql"
  "GraphQL endpoint for LeetCode API."
  :type 'string
  :group 'leetcode-mode)

(defcustom leetcode-lang "python3"
  "Preferred programming language for code templates."
  :type 'string
  :group 'leetcode-mode)

(defcustom leetcode-buffer-name "*LeetCode*"
  "Name of buffer for problem display."
  :type 'string
  :group 'leetcode-mode)

(defvar leetcode--active-requests nil
  "List of active network requests for cancellation.")

(defvar leetcode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l s") #'leetcode-search)
    (define-key map (kbd "C-c l r") #'leetcode-random)
    map)
  "Keymap for LeetCode minor mode.")

;;;###autoload
(define-minor-mode leetcode-mode
  "Global minor mode for LeetCode integration."
  :lighter " LC"
  :keymap leetcode-mode-map
  :global t
  (unless leetcode-mode
    (leetcode--cleanup-requests)))

(defun leetcode--cleanup-requests ()
  "Cancel all pending network requests."
  (mapc #'request-abort leetcode--active-requests)
  (setq leetcode--active-requests nil))

(defun leetcode--graphql-query (query variables callback)
  "Execute GraphQL QUERY with VARIABLES and call CALLBACK on success."
  (let (req)
    (setq body
          (json-encode
           (list
            (cons "query"     query)
            (cons "variables" variables))))
    ;; (message "LeetCode GraphQL payload:\n%s" body)
    (setq req
          (request
            leetcode-api-url
            :type    "POST"
            :headers '(("Content-Type" . "application/json"))
            :data    body
            :parser  'json-read
            :timeout 10
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        ;; `req` is now lexically bound
                        (setq leetcode--active-requests
                              (delq req leetcode--active-requests))
                        (condition-case err
                            (funcall callback (alist-get 'data data))
                          (error
                           (message "Processing error: %s"
                                    (error-message-string err))))))
            :error   (cl-function
                      (lambda (&key error-thrown &allow-other-keys)
                        (setq leetcode--active-requests
                              (delq req leetcode--active-requests))
                        (message "Request failed: %S" error-thrown)))))
    (push req leetcode--active-requests)))

(defun leetcode--fetch-by-slug (slug)
  "Fetch problem DETAILS by SLUG and then display it."
  (let* ((query
          "query($slug:String!) {
             question(titleSlug:$slug) {
               titleSlug questionFrontendId title content hints codeSnippets { langSlug code }
             }
           }")
         (vars   (list (cons "slug" slug)))
         (body   (json-encode `(("query"     . ,query)
                                ("variables" . ,vars))))
         (pr     (make-progress-reporter (format "Loading %s..." slug))))
    ;; (message "fetch-by-slug payload: %s" body)
    (request
      leetcode-api-url
      :type    "POST"
      :headers '(("Content-Type" . "application/json"))
      :data    body
      :parser  'json-read
      :timeout 10
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progress-reporter-done pr)
                  (let ((quest (alist-get 'question (alist-get 'data data))))
                    (leetcode--display-problem quest))))
      :error   (cl-function
                (lambda (&key _error-thrown &allow-other-keys)
                  (progress-reporter-done pr)
                  (message "Network error fetching problem"))))))

;; Render HTML content via shr-render-region
(defun leetcode--render-content(html)
  "Insert HTML into current buffer and render with SHR"
  (let ((start (point)))
    (insert html)
    (shr-render-region start (point))))


(defun leetcode--display-problem (data)
  "Display problem DATA in dedicated buffer."
  (cl-check-type data list)
  (let-alist data
    (with-current-buffer (get-buffer-create leetcode-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "URL: https://leetcode.com/problems/%s/\n\n" .titleSlug))
        (insert (format "#%s. %s\n\n"
                        .questionFrontendId
                        .title))
        (leetcode--render-content .content)

        (when-let ((hlist (append .hints nil)))
          (insert "\n## Hints\n")
          (dolist (hint hlist)
            (insert (format "- %s\n" hint))))

        (when-let ((snip
                    (cl-find-if
                     (lambda (s)
                       (string=
                        (alist-get 'langSlug s)
                        leetcode-lang))
                     .codeSnippets)))
          (insert
           (format "\n## Code Template\n```%s\n%s\n```\n"
                   leetcode-lang
                   (alist-get 'code snip))))

        (goto-char (point-min))
        (special-mode)))
    (display-buffer (get-buffer leetcode-buffer-name))))

;;;###autoload
(defun leetcode-search (query)
  "Search problems matching QUERY."
  (interactive "sSearch LeetCode: ")
  (let ((pr (make-progress-reporter "Searching LeetCode")))
    (leetcode--graphql-query
     "query($search:String!) {
        problemsetQuestionList:questionList(filters:{search:$search}) {
          data { titleSlug title questionFrontendId }
        }
      }"
     `(("search" . ,query))
     (lambda (data)
       (progress-reporter-done pr)
       (let ((rows (alist-get 'data (alist-get 'problemsetQuestionList data))))
         (if (null rows)
             (message "No matches for %S" query)
           (let ((choices
                  (mapcar (lambda (p)
                            (cons
                             (format "%s. %s"
                                     (alist-get 'questionFrontendId p)
                                     (alist-get 'title p))
                             (alist-get 'titleSlug p)))
                          rows))
                 key slug)
             (setq key
                   (completing-read "Select problem: "
                                    (mapcar #'car choices)
                                    nil t))
             (setq slug (alist-get key choices))
             (leetcode--fetch-by-slug slug))))))))


(defconst leetcode--random-query
  "query randomQuestion($categorySlug: String, $filters: QuestionListFilterInput) {
     randomQuestion(categorySlug: $categorySlug, filters: $filters) {
       titleSlug
     }
   }"
  "GraphQL operation for fetching a random LeetCode problem.")

;;;###autoload
(defun leetcode-random (&optional category)
  "Fetch a random LeetCode problem, optionally within CATEGORY."
  (interactive)
  (let* ((query
          "query randomQuestion($categorySlug:String,$filters:QuestionListFilterInput) {
             randomQuestion(categorySlug:$categorySlug, filters:$filters) {
               titleSlug
             }
           }")
         (empty-filters (make-hash-table :test #'equal))
         (vars          (list
                         (cons "categorySlug" (or category ""))
                         (cons "filters"      empty-filters)))
         (body          (json-encode
                         `(("query"         . ,query)
                           ("variables"     . ,vars)
                           ("operationName" . "randomQuestion"))))
         (pr            (make-progress-reporter "Fetching random problem")))
    (message "LeetCode random payload → %s" body)
    (request
      leetcode-api-url
      :type    "POST"
      :headers '(("Content-Type" . "application/json"))
      :data    body
      :parser  'json-read
      :timeout 10

      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progress-reporter-done pr)
                  (let* ((payload (alist-get 'data data))
                         (rq      (alist-get 'randomQuestion payload))
                         (slug    (and rq (alist-get 'titleSlug rq))))
                    (if slug
                        (leetcode--fetch-by-slug slug)
                      (message "LeetCode: no randomQuestion returned")))))

      :error (cl-function
              (lambda (&key _error-thrown &allow-other-keys)
                (progress-reporter-done pr)
                (message "Network error fetching random problem"))))))


(provide 'leetcode-mode)
