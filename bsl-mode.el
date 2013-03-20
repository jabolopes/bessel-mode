;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bsl\\'" . bsl-mode))


; versioning

(defconst bsl-version "v1.0" "`bsl-mode' version number.")

(defun bsl-version ()
  "Echo the current version of `bsl-mode' in the minibuffer."
  (interactive)
  (message "Using bsl-mode version %s" bsl-version))

(defgroup bsl nil
  "Major mode for editing Bsl programs."
  :group 'languages
  :prefix "bsl-")


; comments

(defun bsl-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "--") (comment-end ""))
    (comment-dwim arg)))


; mode maps

(defvar bsl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'bsl-copy-all)
    (define-key map (kbd "C-c C-l") 'bsl-syntax-check)
    (define-key map (kbd "C-c C-r") 'bsl-lookup-lsl-ref)
    (define-key map (kbd "C-c C-g") 'bsl-convert-rgb)
    (define-key map [remap comment-dwim] 'bsl-comment-dwim)
    (define-key map [menu-bar] (make-sparse-keymap))
    map))


; syntax table

(defvar bsl-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\- ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; (modify-syntax-entry ?\| ". 14" table)
    ;; (modify-syntax-entry ?\- ". 23" table)
    table)
  "Syntax table for `bsl-mode'.")

(easy-menu-define bsl-mode-menu bsl-mode-map
  "Menu for the Bsl major mode."
  `("Bsl"
    ["About" (bsl-comment-dwim)]
    "---"
    ;; ["Indent line" indent-according-to-mode]
    ;; ["Indent region" indent-region mark-active]
    ;; ["(Un)Comment region" comment-region mark-active]
    ;; "---"
    ;; ["Start interpreter" switch-to-haskell]
    ;; ["Load file" inferior-haskell-load-file]
    ;; "---"
    ;; ,(if (default-boundp 'eldoc-documentation-function)
    ;;      ["Doc mode" eldoc-mode
    ;;       :style toggle :selected (bound-and-true-p eldoc-mode)]
    ;;    ["Doc mode" haskell-doc-mode
    ;;     :style toggle :selected (and (boundp 'haskell-doc-mode) haskell-doc-mode)])
    ["Customize" (customize-group 'bsl)]
    ))


; font locking

(setq bsl-keywords '("as" "def" "me" "module" "nrdef" "type" "use" "where"))
(setq bsl-types '("Bool" "Int" "Real" "Char"))
(setq bsl-constants '("false" "true"))
(setq bsl-events '())
(setq bsl-functions '())

(setq bsl-keywords-re (regexp-opt bsl-keywords 'words))
(setq bsl-punctuation-re "@\\|:=")
(setq bsl-type-re "\\b[[:upper:]][[:alnum:]'_]*\\b")
(setq bsl-constant-re (regexp-opt bsl-constants 'words))
(setq bsl-event-re (regexp-opt bsl-events 'words))
(setq bsl-functions-re (regexp-opt bsl-functions 'words))

(setq bsl-font-lock-keywords
  `(("\"\\.\\*\\?" . font-lock-string-face)
    (,bsl-punctuation-re . font-lock-variable-name-face)
    ("\\(def\\|nrdef\\) \\(\\(\\w\\|\\s_\\)+\\)" (2 font-lock-function-name-face))
    (,bsl-type-re . font-lock-type-face)
    (,bsl-constant-re . font-lock-constant-face)
    (,bsl-event-re . font-lock-builtin-face)
    (,bsl-functions-re . font-lock-function-name-face)
    (,bsl-keywords-re . font-lock-keyword-face)
  ))


;;;###autoload
(define-derived-mode bsl-mode fundamental-mode "Bsl"
  "Major mode for editing Bsl"
  :syntax-table bsl-syntax-table
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'font-lock-defaults) '((bsl-font-lock-keywords)))

  ;(set (make-local-variable 'indent-line-function) 'sample-indent-line)
)


; indentation

(defun sample-indent-line ()
   "Indent current line of Sample code."
   (interactive)
   (let ((savep (> (current-column) (current-indentation)))
	  (indent (condition-case nil (max (sample-calculate-indentation) 0)
		       (error 0))))
     (if savep
	  (save-excursion (indent-line-to indent))
       (indent-line-to indent))))

 (defun sample-calculate-indentation ()
   "Return the column to which the current line should be indented."
   0)


(provide 'bsl-mode)