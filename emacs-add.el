
;; emacs-lisp file for customizing emacs.
;;
;; Usage :
;;   call `load' function with path to this file
;;
;;      (load path/to/emacs-add.el)
;;
;;   in ~/.emacs or ~/.emacs.d/init.el, or run emacs with option
;;
;;     emacs -l path/to/emacs-add.el
;;
;; Some plugins are required from Github or other repositories.
;;

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(defun is-win () (eq system-type 'windows-nt))
(defun is-linux () (eq system-type 'gnu/linux))
(defun is-mac () (eq system-type 'darwin))

(defconst emacs-add-dir
  (let* ((emacs-add load-file-name))
    (if emacs-add (file-name-directory emacs-add)
      (error "Not found emacs-add directory.
Usage :
    (load path/to/emacs-add.el)
in ~/.emacs or ~/.emacs.d/init.el, or
    emacs -l path/to/emacs-add.el
by command. "))))

(message "emacs-add: %s" emacs-add-dir)

(require 'package)
(dolist (packs (list
                (cons "melpa" "http://melpa.milkbox.net/packages/")
                (cons "marmalade" "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives packs))
(package-initialize)

(when (is-win) (set-frame-font "Consolas"))
(when (is-linux)
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(require 'cl)

;; japanese input method
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
  (if (not (require 'mozc nil t))
      (message "Unable to load mozc.")
    (setq default-input-method "japanese-mozc")))

;; ----- el utility -----
(defun buffer-mode (buffer-or-string)
  "Get major mode of paticular buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun eval-sexp-forwardn (&optional n)
  (interactive "P")
  (let ((mode (buffer-mode (current-buffer))))
    (if (not (or (eq mode 'emacs-lisp-mode)
                 (eq mode 'lisp-interaction-mode)))
        (message "The buffer is not elisp.")
      (let ((n1 (if (null n) 1 n)))
        (loop repeat n1
              do (let ((b (point))
                       (e (progn (forward-sexp) (point))))
                   (eval-region b e)))))))

(global-set-key (kbd "C-; C-e") 'eval-sexp-forwardn)
(global-set-key (kbd "C-; e") 'eval-sexp-forwardn)

;; ----- unbinded keys -----
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "ESC ESC ESC"))
(setq inhibit-startup-message t)

;; ----- frames, windows -----
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq default-frame-alist
      (append '((top . 0)
                (width . 120)
                (height . 57)
                (background-color . "#F4F5F9"))))

(set-face-attribute 'default nil :height 90)

(show-paren-mode t)
(set-face-attribute 'show-paren-match nil :background "#73d0b8")
(set-face-attribute 'show-paren-mismatch nil :background "#f43050")

(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)

(global-set-key
 (kbd "C-x C-3")
 (lambda (&optional n)
   (interactive "P")
   (let ((n1 (if (null n) 2 (- n 1))))
     (loop repeat n1
           do (split-window-horizontally (- (window-width) 10)))
     (balance-windows-area))))

(global-set-key
 (kbd "C-x M-3")
 (lambda (&optional n)
   (interactive "P")
   (let* ((n1 (if (null n) 2 (- n 1)))
          (h (/ (window-height) 2))
          (w (/ (window-width) (+ n1 1))))
     (loop repeat n1
           do (progn
                (split-window-horizontally (- (window-width) w))
                (windmove-right)
                (split-window-vertically h)
                (windmove-left)))
     (split-window-vertically h))))

(global-set-key (kbd "S-<up>") 'scroll-down-line)
(global-set-key (kbd "S-<down>") 'scroll-up-line)

(global-set-key (kbd "C-x w b") 'balance-windows-area)

(defun toggle-window-dedicated ()
  (interactive)
  (message "%s dedicated: %s"
           (buffer-name)
           (set-window-dedicated-p
            (get-buffer-window)
            (not (window-dedicated-p)))))

(global-set-key (kbd "C-x w d") 'toggle-window-dedicated)

(defun register-clear-list () (interactive) (setq register-alist '()))

;; ----- display, faces -----
(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :background "#cef0e9")

(setq-default truncate-lines t)
(defun truncate-lines-off () (setq truncate-lines nil))
(global-set-key (kbd "C-x g l") (lambda () (interactive (toggle-truncate-lines))))

(setq-default column-number-mode t)

(global-linum-mode t)
(defun linum-mode-off () (linum-mode 0))

;; turncate lines off
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook
                'cider-repl-mode-hook
                'compilation-mode-hook
                'messages-buffer-mode-hook
                'help-mode-hook
                'completion-list-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (truncate-lines-off))))

(let ((mb (get-buffer "*Messages*"))
      (req (require 'simple nil t)))
  (when (and mb req (functionp 'messages-buffer-mode))
    (set-buffer mb)
    (messages-buffer-mode)))

;; line number mode off
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (linum-mode-off))))

(set-face-attribute 'region nil :background "#a9dcff")

;; ----- interpreter -----
;; inferior emacs lisp mode
(global-set-key (kbd "C-/ C-i") 'ielm)

;; ipython
(when (and (eq system-type 'gnu/linux)
           (or (file-exists-p "/usr/bin/ipython")
               (file-exists-p "/usr/local/bin/ipython")))
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "\\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; ----- desktop -----
(desktop-save-mode t)
(global-set-key (kbd "C-/ C-d") 'desktop-change-dir)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (setq search-ring nil)
            (setq regexp-search-ring nil)
            (setq file-name-history nil)
            (if (not (and (require 'misc-desktop nil t) (functionp 'misc-init-desktop)))
                (message "Unable to load misc-desktop.")
              (misc-init-desktop))))

(defun variables-to-desktop-globals (&rest var)
  (mapc (lambda (v)
          (when (and v (boundp v))
            (add-to-list 'desktop-globals-to-save v)
            (message "%s added to desktop-globals-to-save." v)))
        var))

;; ----- project, directories -----
;; ebrowse tree/member key map
(if (not (require 'ebrowse))
    (message "Unable to load ebrowse.")
  (when (boundp 'ebrowse-tree-mode-map)
    (let ((map ebrowse-tree-mode-map))
      (setf ebrowse-tree-mode-map map)
      (define-key map (kbd "C-l") nil)
      (define-key map (kbd "S r") 'ebrowse-redraw-tree)
      (define-key map (kbd "n") 'next-line)
      (define-key map (kbd "p") 'previous-line)
      (define-key map (kbd "f") 'ebrowse-find-class-declaration)
      (let ((sup (make-sparse-keymap)))
        (suppress-keymap sup t)
        (define-key map (kbd "s") sup)
        (define-key sup (kbd "F") 'ebrowse-tree-command:show-static-member-functions)
        (define-key sup (kbd "f") 'ebrowse-tree-command:show-member-functions)
        (define-key sup (kbd "V") 'ebrowse-tree-command:show-static-member-variables)
        (define-key sup (kbd "v") 'ebrowse-tree-command:show-member-variables)
        (define-key sup (kbd "t") 'ebrowse-tree-command:show-types)
        (define-key sup (kbd "r") 'ebrowse-tree-command:show-friends))))

  (when (boundp 'ebrowse-member-mode-map)
    (let ((map ebrowse-member-mode-map))
      (setf ebrowse-member-mode-map map)
      (define-key map (kbd "C-l") nil)
      (define-key map (kbd "S r") 'ebrowse-redisplay-member-buffer)
      (define-key map (kbd "n") 'next-line)
      (define-key map (kbd "p") 'previous-line)
      (let ((sup (make-sparse-keymap)))
        (suppress-keymap sup t)
        (define-key map (kbd "s") sup)
        (define-key sup (kbd "l") 'ebrowse-toggle-long-short-display)
        (define-key sup (kbd "d") 'ebrowse-switch-member-buffer-to-derived-class)
        (define-key sup (kbd "F") 'ebrowse-display-static-functions-member-list)
        (define-key sup (kbd "f") 'ebrowse-display-function-member-list)
        (define-key sup (kbd "V") 'ebrowse-display-static-variables-member-list)
        (define-key sup (kbd "v") 'ebrowse-display-variables-member-list)
        (define-key sup (kbd "t") 'ebrowse-display-types-member-list)
        (define-key sup (kbd "r") 'ebrowse-display-friends-member-list)
        ))))

;; hs-minor-mode
(defun set-hs-minor-mode-map ()
  (if (not (boundp 'hs-minor-mode-map))
      (message "Could not set hs-minor-mode-map.")
    (if (and (boundp hs-minor-mode) hs-minor-mode)
        (let* ((map hs-minor-mode-map)
               (_ (setf hs-minor-mode-map map))
               (sup (make-sparse-keymap)))
          (suppress-keymap sup t)
          (define-key map (kbd "C-c h s") sup)
          (define-key sup (kbd "h") 'hs-hide-block)
          (define-key sup (kbd "s") 'hs-show-block)
          (define-key sup (kbd "g") 'hs-toggle-hiding)
          (define-key sup (kbd "c") 'hs-hide-all)
          (define-key sup (kbd "e") 'hs-show-all)))))

(if (not (require 'hideshow nil t))
    (message "Unable to load hideshow.")
  (add-hook 'hs-minor-mode-hook 'set-hs-minor-mode-map))

;; direx original - https://github.com/m2ym/direx-el
;;(unless (package-installed-p 'direx)
;;  (package-install 'direx))
(add-to-list 'load-path (concat emacs-add-dir "direx-el"))
(if (not (and (require 'direx nil t) (require 'direx-project nil t)))
    (message "Unable to load direx, direx-project.")

  (defun direx:find-item-noselect (&optional item readonly)
    (interactive)
    (let* ((item (or item (direx:item-at-point))))
      (if (direx:item-leaf-p item)
          (let ((buf (find-file-noselect
                      (direx:file-full-name (direx:item-tree item)))))
            (when readonly
              (set-buffer buf)
              (read-only-mode t))))))

  (defun direx:find-item-noselect-readonly (&optional item)
    (interactive)
    (direx:find-item-noselect item t))

  (let ((map direx:direx-mode-map))
    (define-key map (kbd "s f") 'direx:find-item-noselect)
    (define-key map (kbd "s r") 'direx:find-item-noselect-readonly)
    (setq direx:direx-mode-map map))

  (global-set-key (kbd "C-/ j g") 'direx-project:jump-to-project-root)
  (global-set-key (kbd "C-/ j d") 'direx:find-directory))

;; magit - https://github.com/magit/magit
;; depends on git-modes - https://github.com/magit/git-modes
(add-to-list 'load-path (concat emacs-add-dir "git-modes"))
(add-to-list 'load-path (concat emacs-add-dir "magit"))
(if (not (require 'magit nil t))
    (message "Unable to load magit.")
  (global-set-key (kbd "C-<f6>") 'magit-status))
;;(unless (package-installed-p 'magit)
;;  (package-install 'magit))

;; ----- searching -----
;; highlighting
(global-set-key (kbd "C-x w h") 'highlight-regexp)
(global-set-key (kbd "C-x w r") 'unhighlight-regexp)

(defun exec-unhighlight-all ()
  (mapcar (lambda (p)
            (unhighlight-regexp (car p)))
          hi-lock-interactive-patterns))

(defun unhighlight-all (&optional force)
  (interactive "P")
  (message "%s" force)
  (if (not hi-lock-interactive-patterns)
      (message "No highlighting to remove.")
    (if (or force (y-or-n-p "Unhighlight all patterns."))
        (exec-unhighlight-all))))

(global-set-key (kbd "C-x w u") 'unhighlight-all)

;; multi buffer
(add-to-list 'load-path (concat emacs-add-dir "buffer-collect"))
(if (not (require 'buffer-collect nil t))
    (message "Unable to load buffer-collect.")
  (defun multi-occur-ex (&optional regexp type-str)
    (interactive (list (read-regexp "Regexp: ")
                       (bc:type-list-str-prompt)))
    (if (or (not regexp) (string-equal regexp ""))
        (error "Cannot execute with empty regexp.")
      (multi-occur (bc:collect-buffers type-str) regexp)))

  (global-set-key (kbd "C-x <f3>") 'multi-occur-ex))

;; ----- edit mode, editing support -----
;; google-c-style - https://code.google.com/p/google-styleguide/
(add-to-list 'load-path (concat emacs-add-dir "google-c-style"))
(if (not (require 'google-c-style nil t))
    (message "Unable to load google-c-style.")
  (add-hook 'c-mode-common-hook
            (lambda (&optional opt)
              (google-set-c-style))))
;;(unless (package-installed-p 'google-c-style)
;;  (package-install 'google-c-style))

;; gyp mode - https://code.google.com/p/gyp/
;; license of source files is NEW BSD LICENSE
(let* ((system system-type)
       (gyp-path
        (cond ((eq system 'windows-nt) (concat emacs-add-dir "../emacs-plugins/gyp"))
              ((eq system 'gnu/linux) (concat emacs-add-dir "gyp")))))
  (add-to-list 'load-path gyp-path))
(if (not (require 'gyp nil t))
    (message "Unable to load gyp."))

;; auto-complete - https://github.com/auto-complete/auto-complete
;; auto-complete dependes on poup-el - https://github.com/auto-complete/popup-el
(add-to-list 'load-path (concat emacs-add-dir "popup-el"))
(add-to-list 'load-path (concat emacs-add-dir "auto-complete"))
(if (not (require 'auto-complete nil t))
    (message "Unable to load auto-complete.")
  (global-auto-complete-mode nil))
;;(unless (package-installed-p 'auto-complete)
;;  (package-install 'auto-complete))

;; csharp-mode - http://www.emacswiki.org/emacs/CSharpMode
(add-to-list 'load-path (concat emacs-add-dir "csharp-mode"))
(if (not (require 'csharp-mode nil t))
    (message "Unable to load csharp-mode."))

;; paredit - http://mumble.net/~campbell/git/paredit.git/
;;         - https://github.com/emacsmirror/paredit
(add-to-list 'load-path (concat emacs-add-dir "paredit"))
(if (not (require 'paredit nil t))
    (message "Unable to load paredit.")
  ;; unbinded keys M-<down>, M-<up>, C-M-<down>, C-M-<up>, C-M-f, C-M-b
  (dolist (hooks (list
                  'emacs-lisp-mode-hook
                  'eval-expression-minibuffer-setup-hook
                  'ielm-mode-hook
                  'lisp-mode-hook
                  'lisp-interaction-mode-hook
                  'scheme-mode-hook
                  'clojure-mode-hook
                  ))
    (add-hook hooks 'enable-paredit-mode)))

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 2)))

(when (and (is-linux)
           (eq (call-process-shell-command "which flake8") 0)
           (require 'tramp nil t)
           (require 'flymake nil t)
           (require 'python nil t))
  (defun flymake-flake8-init ()
    (let* ((temp-file
            (flymake-init-create-temp-buffer-copy
             'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file (file-name-directory buffer-file-name))))
      (list "flake8" (list local-file "--ignore=E111,E121,E501"))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-flake8-init))
  (add-hook 'python-mode-hook
            (lambda (&optional opt)
              (flymake-mode t)))
  (setq python-mode-map
        (let ((map python-mode-map))
          (define-key map (kbd "C-c e")
            'flymake-display-err-menu-for-current-line)
          (define-key map (kbd "C-c f")
            'flymake-goto-next-error)
          (define-key map (kbd "C-c b")
            'flymake-goto-prev-error)
          map)))

;; clojure-mode - https://github.com/clojure-emacs/clojure-mode
;; contains:
;;   clojure-test-mode
;;   dependes:
;;     cider (see below)
;;
;; cider - https://github.com/clojure-emacs/cider/
;; dependencis
;;  clojure-mode (see above)
;;  dash - https://github.com/magnars/dash.el
;;  pkg-info.el - https://github.com/lunaryorn/pkg-info.el
;;  pridictive - git clone http://www.dr-qubit.org/git/predictive.git
;;  dependencis:
;;    epl - https://github.com/cask/epl
;;
(add-to-list 'load-path (concat emacs-add-dir "epl"))
(add-to-list 'load-path (concat emacs-add-dir "dash.el"))
(add-to-list 'load-path (concat emacs-add-dir "pkg-info.el"))
(add-to-list 'load-path (concat emacs-add-dir "predictive"))
(add-to-list 'load-path (concat emacs-add-dir "clojure-mode"))
(add-to-list 'load-path (concat emacs-add-dir "cider"))
(dolist (packs (list 'clojure-mode
                     'cider
                     'cider-macroexpansion))
  ((lambda (p)
     (unless (require p nil t) (message "Unable to load %s" p)))
   packs))

;; scala-mode - https://github.com/hvesalai/scala-mode2
;; sbt-mode   - https://github.com/hvesalai/sbt-mode
(dolist (dirs '("scala-mode2" "sbt-mode"))
  (add-to-list 'load-path (concat emacs-add-dir dirs)))
(if (not (require 'scala-mode2 nil t))
    (message "Unable to load scala-mode2."))
(if (not (require 'sbt-mode nil t))
    (message "Unable to load sbt-mode."))

;; qml-mode - https://github.com/Kikuchi-M/qml-mode
(add-to-list 'load-path (concat emacs-add-dir "qml-mode"))
(if (not (require 'qml-mode nil t))
    (message "Unable to load qml-mode.")
  (require 'qml-tools nil t))

;; g-compile - https://github.com/Kikuchi-M/g-compile
(add-to-list 'load-path (concat emacs-add-dir "g-compile"))
(if (not (require 'g-compile nil t))
    (message "Unable to load g-compile.")
  (variables-to-desktop-globals 'g-compile-repository-dir
                                'g-compile-projects)
  (global-set-key (kbd "C-/ h") 'g-compile-runhooks)
  (global-set-key (kbd "C-/ y") (lambda () (interactive) (g-compile-runhooks t)))
  (global-set-key (kbd "C-/ r") 'g-compile-ninja)
  (global-set-key (kbd "C-/ e") (lambda () (interactive) (g-compile-ninja t))))

;; tab, indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun indent-all-lines ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-<tab>") 'indent-all-lines)

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-; C-s") 'replace-string)
(global-set-key (kbd "C-; C-r") 'replace-regexp)
(global-set-key (kbd "C-; C-q") 'query-replace)
(global-set-key (kbd "C-; C-x") 'query-replace-regexp)

(global-set-key (kbd "C-; c") 'comment-region)
(global-set-key (kbd "C-; u") 'uncomment-region)

(require 'cc-cmds)
(global-set-key (kbd "C-; d") 'c-hungry-delete-forward)
(global-set-key (kbd "C-; C-d") 'c-hungry-delete-forward)
(global-set-key (kbd "C-; b") 'c-hungry-delete-backwards)
(global-set-key (kbd "C-; C-b") 'c-hungry-delete-backwards)

;; Remove spaces in back of each line.
(global-set-key (kbd "C-; C-<SPC>") 'delete-trailing-whitespace)

(defun replace-regexp-all (regexp to-string)
  (save-excursion
    (goto-char 0)
    (replace-regexp regexp to-string)))

(defun back-to-bol-or-nonspace (&optional n)
  (let ((n1 (if (not (numberp n)) 1 n)))
    (when (> n1 0)
      (search-backward-regexp "^\\|[^[:space:]]")
      (back-to-bol-or-nonspace (- n1 1)))))

(defun remove-auto-inserted-spaces (cont)
  (when cont
    (let* ((hit (search-forward-regexp " [[\\(]" nil t))
           (bop (when (and hit
                           (let ((parse (parse-partial-sexp (point-at-bol) hit)))
                             (and (not (nth 3 parse)) (not (nth 4 parse)))))
                  (backward-char) (point)))
           (word (if (or (not bop)
                         (string-match
                          "[\\.=;:,&|^/*+-]"
                          (char-to-string (save-excursion (char-after (back-to-bol-or-nonspace)))))
                         (string-match
                          "^[ \t]+$"
                          (let ((bol-to-bop (buffer-substring (point-at-bol) bop)))
                            bol-to-bop)))
                     nil
                   (message "getting previous word")
                   (save-excursion (backward-word) (word-at-point)))))
      (when (and word (not (string-match "^\\(return\\|void\\|if\\|switch\\|when\\|for\\)$" word)))
        (c-hungry-delete-backwards))
      (remove-auto-inserted-spaces hit))))

(global-set-key
 (kbd "C-; C-8")
 (lambda (&optional opt)
   "Remove spaces inserted automatically by paredit.
This can execute in major modes of c family or qml-mode."
   (interactive)
   (let* ((m (buffer-mode (current-buffer))))
     (if (or (eq m 'qml-mode)
             (c-major-mode-is m)
             (eq m 'js-mode)
             (eq m 'csharp-mode)
             (eq m 'python-mode))
         (save-excursion
           (goto-char (point-min))
           (remove-auto-inserted-spaces t))
       (message "The buffer mode is not compatible.")))))

(defun downcase-char (&optional n)
  (interactive "P")
  (let* ((n1 (if (and n (numberp n)) n 1))
         (b (point))
         (e (+ b n1)))
    (downcase-region b e)
    (goto-char e)))

(dolist (keys (list (kbd "C-; w")
                    (kbd "C-; C-w")))
  (global-set-key
   keys
   (lambda (&optional n)
     (interactive "p")
     (save-excursion
       (copy-region-as-kill (point) (progn (forward-word n) (point)))))))

;; c++ mode
(if (not (and (require 'cc-mode nil t)
              (require 'find-file nil t)
              c++-mode-map
              cc-other-file-alist))
    (message "Unable to load cc-mode.")

  (dolist (types (list
                  '("\\.h\\'" . c++-mode)
                  '("\\.ipp\\'" . c++-mode)
                  '("\\.tcc\\'" . c++-mode)))
    (add-to-list 'auto-mode-alist types))

  (dolist (modes (list c++-mode-map
                       c-mode-map))
    (funcall (lambda (mode)
               (let ((m mode))
                 (define-key m (kbd "<f6>") 'ff-get-other-file)
                 (setq mode m)))
             modes))

  (defun add-cc-other-assoc (key others)
    (if (not (and key others
                  (or (stringp others)
                      (reduce (lambda (f e) (and f (stringp e)))
                              others
                              :initial-value t))))
        (message "Could not add association to cc-other-file-alist. %s %s"
                 key others)
      (let* ((assoc (assoc key cc-other-file-alist))
             (new-alist (remove assoc cc-other-file-alist))
             (to-add (if (stringp others) (list others) others))
             )
        (setq cc-other-file-alist
              (cons (list key
                          (if assoc
                              (reduce (lambda (l e)
                                        (if (member e l) l
                                          (append l (list e))))
                                      to-add
                                      :initial-value (cadr assoc))
                            to-add))
                    new-alist)))))

  (add-cc-other-assoc "\\.h\\'" (list ".ipp" ".tcc"))
  (add-cc-other-assoc "\\.ipp\\'" (list ".h" ".cpp"))
  (add-cc-other-assoc "\\.tcc\\'" (list ".h" ".cpp"))
  )

;; ----- compilation, debug -----
(when (boundp 'compilation-mode-map)
  (let ((map compilation-mode-map))
    (setf compilation-mode-map map)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)))

;; gdb-debugger - https://github.com/Kikuchi-M/gdb-debugger
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path (concat emacs-add-dir "gdb-debugger"))
  (if (not (require 'gdb-debugger nil t))
      (message "Unable to load gdb-debugger.")
    (variables-to-desktop-globals 'gdb-debugger-executable-list)))


;; ----- private utility -----
(add-to-list 'load-path (concat emacs-add-dir "emacs-private"))
(unless (require 'misc nil t)
  (message "Unable to load misc."))
