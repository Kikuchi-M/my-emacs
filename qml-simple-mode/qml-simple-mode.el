;; Simple QML Mode

(defvar qml-simple-mode-hook nil)

(let ((qml-highlight-blue "MediumBlue")      ;#0000cd
      (qml-highlight-orchid "DarkOrchid")    ;#9932cc
      (qml-highlight-olive "OliveDrab")      ;#6b8e23
      (qml-highlight-red "red4")             ;#8b0000
      )

  (defface qml-specifier-face
    `((t :foreground ,qml-highlight-blue))
    "Face for element specifier.")
  (defvar qml-specifier-face 'qml-specifier-face)

  (defface qml-preprocessor-face
    `((t :foreground ,qml-highlight-orchid))
    "Face for preprocessor.")
  (defvar qml-preprocessor-face 'qml-preprocessor-face)

  (defface qml-package-face
    `((t :foreground ,qml-highlight-olive))
    "Face for package name.")
  (defvar qml-package-face 'qml-package-face)

  (defface qml-package-version-face
    `((t :foreground ,qml-highlight-red))
    "Face for package version.")
  (defvar qml-package-version-face 'qml-package-version-face)
  )

(defvar qml-simple-syntax-table
  (let ((qml-st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" qml-st)
    (modify-syntax-entry ?* ". 23" qml-st)
    (modify-syntax-entry ?\n "> b" qml-st)
    (modify-syntax-entry ?' "\"" qml-st)
    qml-st)
  "Syntax table for qml-simple-mode.")

(defvar qml-font-lock-keywords
  (let* ((separator "\\|")
         (qml-directive-kwd
          (mapconcat 'identity '("import" "using") separator))
         )
    (list
     ;; preprocessor
     (list (concat "^[ \t]*\\(" qml-directive-kwd "\\)[ \t]+"
                   "\\("
                   "\\(\\(\\([a-zA-Z][a-zA-Z0-9]*\\)\\.?\\)*\\([a-zA-Z][a-zA-Z0-9]*\\)\\)" ;; packages
                   "[ \t]+"
                   "\\(\\(\\([0-9]+\\)\\.?\\)*\\([0-9]+\\)\\)" ;; version
                   "\\|"
                   "\\(\"[^ ]+\"\\)" ;; directory
                   "\\)"
                   "[ \t]*;?$")
           '(1 qml-preprocessor-face nil t)
           '(3 qml-package-face nil t)
           '(7 qml-package-version-face nil t))

     ;; specifiers
     (list (concat "\\(^\\|\\*/\\|:[ \t]*\\)"
                   "[ \t]*"
                   "\\([A-Z][a-zA-Z0-9]*\\)[ \t]*\\({\\|$\\)")
           2 qml-specifier-face)

     ;; props
     (list (concat "\\(^[ \t]*\\|;[ \t]*\\|{[ \t]*\\)"
                   "\\(\\([a-zA-Z][a-zA-Z0-9]*\\)\\.\\)?\\([a-z][a-zA-Z0-9]*\\)"
                   "[ \t]*[:{]")
           '(3 font-lock-variable-name-face nil t)
           '(4 font-lock-variable-name-face nil t))
     )))

(defvar qml-indent-offset 2)

(defun qml-indent-line ()
  "Indent current line according to QML indentation rule."
  (interactive)
  (display-message-or-buffer "*** qml-simple-mode *** not implemented qml-indent-line function.")
  )

(define-derived-mode qml-simple-mode fundamental-mode "QML"
  "Mejor mode for Qt declarative UI (simple mode)"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table qml-simple-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(qml-font-lock-keywords))
  (set (make-local-variable 'tab-width) qml-indent-offset)
  ;;(set (make-local-variable 'indent-line-function) 'qml-indent-line)
  (setq major-mode 'qml-simple-mode)
  (setq mode-name "QML")
  (run-hooks 'qml-simple-mode-hook)
  )

(provide 'qml-simple-mode)
