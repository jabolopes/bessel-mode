;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bsl\\'" . bessel-mode))


; versioning

(defconst bsl-version "1.0" "`bessel-mode' version number.")

(defun bsl-version ()
  "Echo the current version of `bessel-mode' in the minibuffer."
  (interactive)
  (message "bessel-mode version %s" bsl-version))

(defgroup bsl nil
  "Major mode for editing Bessel programs."
  :group 'languages
  :prefix "bessel-")


; comments

(defun bsl-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "-- ") (comment-end ""))
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
  "Syntax table for `bessel-mode'.")

(easy-menu-define bsl-mode-menu bsl-mode-map
  "Menu for the Bessel major mode."
  `("Bessel"
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

(defcustom bsl-operators
  '("\*" "\/" "+" "-" "==" "/=" "<" "<=" ">" ">=")
  "Operators for `bessel-mode'."
  :group 'bsl
  :type '(repeat string))

(setq bsl-operators-paren
      (append
       bsl-operators
       (mapcar (lambda (str) (concat "(" str ")")) bsl-operators)))

(defcustom bsl-punctuation
  '("@" ":" "=" "|" "->" "<-" "&&" "||")
  "Punctuation for `bessel-mode'."
  :group 'bsl
  :type '(repeat string))

(defcustom bsl-constants
  '("false" "true")
  "Constants for `bessel-mode'."
  :group 'bsl
  :type '(repeat string))

(defcustom bsl-keywords
  '("as" "cotype" "def" "me" "module" "nrdef" "sig" "type" "use" "where")
  "Keywords for `bessel-mode'."
  :group 'bsl
  :type '(repeat string))

(setq bsl-string-re "\"\\.\\*\\?")
(setq bsl-composition-re (regexp-opt '("o" "(o)") 'words))
(setq bsl-operators-re (regexp-opt bsl-operators-paren))
(setq bsl-punctuation-re (regexp-opt bsl-punctuation))
(setq bsl-top-decl "\\(def\\|sig\\) \\(\\(\\w\\|\\s_\\)+\\)")
(setq bsl-types-re "\\b[[:upper:]][[:alnum:]'_]*\\b")
(setq bsl-constants-re (regexp-opt bsl-constants 'words))
(setq bsl-keywords-re (regexp-opt bsl-keywords 'words))

(setq bsl-font-lock-keywords
  `((,bsl-string-re . font-lock-string-face)
    (,bsl-top-decl (2 font-lock-function-name-face))
    (,bsl-composition-re . font-lock-variable-name-face)
    (,bsl-operators-re . font-lock-variable-name-face)
    (,bsl-punctuation-re . font-lock-variable-name-face)
    (,bsl-types-re . font-lock-type-face)
    (,bsl-constants-re . font-lock-constant-face)
    (,bsl-keywords-re . font-lock-keyword-face)))


;;;###autoload
(define-derived-mode bessel-mode fundamental-mode "Bessel"
  "Major mode for editing Bessel"
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


(provide 'bessel-mode)
