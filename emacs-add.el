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

(setq default-buffer-file-coding-system 'utf-8)

(when (eq system-type 'windows-nt) (set-frame-font "Consolas"))
(when (eq system-type 'gnu/linux)
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(require 'cl)

;; japanese input method
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
  (if (not (require 'mozc nil t))
      (message "Unable to load mozc.")
    (setq default-input-method "japanese-mozc")))

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

;; ----- display, faces -----
(global-hl-line-mode t)

(setq-default truncate-lines t)
(defun truncate-lines-off () (setq truncate-lines nil))

(setq-default column-number-mode t)

(global-linum-mode t)
(defun linum-mode-off () (linum-mode 0))

;; turncate lines off
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook
                'messages-buffer-mode-hook
                'help-mode-hook
                'completion-list-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (truncate-lines-off))))

(let ((mb (get-buffer "*Messages*")))
  (when mb
    (set-buffer mb)
    (messages-buffer-mode)))

;; line number mode off
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (linum-mode-off))))

(set-face-attribute 'region nil :background "SteelBlue1")

;; ----- interpreter -----
(global-set-key (kbd "C-/ C-i") 'ielm)

;; ----- project, directories -----
;; direx original - https://github.com/m2ym/direx-el
;;(unless (package-installed-p 'direx)
;;  (package-install 'direx))
(add-to-list 'load-path (concat emacs-add-dir "direx-el"))
(if (not (and (require 'direx nil t) (require 'direx-project nil t)))
    (message "Unable to load direx, direx-project.")

  (defun direx:find-item-noselect (&optional item)
    (interactive)
    (let* ((item (or item (direx:item-at-point))))
      (if (direx:item-leaf-p item)
          (find-file-noselect
           (direx:file-full-name (direx:item-tree item))))))

  (let ((map direx:direx-mode-map))
    (define-key map (kbd "s") 'direx:find-item-noselect)
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

(add-to-list 'load-path (concat emacs-add-dir "buffer-collect"))
(if (not (require 'buffer-collect nil t))
    (message "Unable to load buffer-collect.")
  (defun multi-occur-ex (&optional type-str regexp)
    (interactive (list (bc:type-list-str-prompt)
                       (read-regexp "Regexp: ")))
    (if (or (not regexp) (string-equal regexp ""))
        (error "Cannot execute with empty regexp.")
      (multi-occur (bc:collect-buffers type-str) regexp))))

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

;; auto-complete - https://github.com/auto-complete/auto-complete
;; auto-complete dependes on poup-el - https://github.com/auto-complete/popup-el
(add-to-list 'load-path (concat emacs-add-dir "popup-el"))
(add-to-list 'load-path (concat emacs-add-dir "auto-complete"))
(if (not (require 'auto-complete nil t))
    (message "Unable to load auto-complete.")
  (global-auto-complete-mode 1))
;;(unless (package-installed-p 'auto-complete)
;;  (package-install 'auto-complete))

;; paredit - http://mumble.net/~campbell/git/paredit.git/
;;         - https://github.com/goncha/paredit
(add-to-list 'load-path (concat emacs-add-dir "paredit"))
(if (not (require 'paredit nil t))
    (message "Unable to load paredit.")
  ;; unbinded keys M-<down>, M-<up>, C-M-<down>, C-M-<up>, C-M-f, C-M-b
  (dolist (hooks (list
                  'emacs-lisp-mode-hook
                  'eval-expression-minibuffer-setup-hook
                  'sh-mode-hook
                  'ielm-mode-hook
                  'lisp-mode-hook
                  'lisp-interaction-mode-hook
                  'scheme-mode-hook
                  'c-mode-hook
                  'c++-mode-hook))
    (add-hook hooks 'enable-paredit-mode)))

;; qml-simple-mode - https://github.com/Kikuchi-M/qml-mode
(add-to-list 'load-path (concat emacs-add-dir "qml-mode"))
(if (not (require 'qml-mode nil t))
    (message "Unable to load qml-mode.")
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
  (when (functionp 'enable-paredit-mode)
    (add-hook 'qml-mode-hook 'enable-paredit-mode)))

;; tab, indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-; C-s") 'replace-string)
(global-set-key (kbd "C-; C-r") 'replace-regexp)
(global-set-key (kbd "C-; C-q") 'query-replace)
(global-set-key (kbd "C-; C-x") 'query-replace-regexp)

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

(global-set-key
 (kbd "C-; C-8")
 (lambda (&optional opt)
   "Remove spaces inserted automatically by paredit.
This can execute in major modes of c family or qml-mode."
   (interactive)
   (let* ((m (buffer-mode (current-buffer))))
     (if (or (eq m 'qml-mode) (c-major-mode-is m))
         (replace-regexp-all "\\([^=:&|^/*+-]\\) +\\(\\[\\|(\\) *\\([^ ]?.*[^ ]?\\)" "\\1\\2\\3")
       (message "The buffer mode is not compatible.")))))

(defun downcase-char (&optional n)
  (interactive "P")
  (let* ((n1 (if (and n (numberp n)) n 1))
         (b (point))
         (e (+ b n1)))
    (downcase-region b e)
    (goto-char e)))

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

;; ----- el utility -----
(defun buffer-mode (buffer-or-string)
  "Get major mode of paticular buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun eval-sexp-forwardn (&optional n)
  (interactive "P")
  (if (not (eq (buffer-mode (current-buffer)) 'emacs-lisp-mode))
      (message "The buffer is not elisp.")
    (let ((n1 (if (null n) 1 n)))
      (loop repeat n1
            do (let ((b (point))
                     (e (progn (forward-sexp) (point))))
                 (eval-region b e))))))

(global-set-key (kbd "C-; C-e") 'eval-sexp-forwardn)
(global-set-key (kbd "C-; e") 'eval-sexp-forwardn)

;; ----- private utility -----
(add-to-list 'load-path (concat emacs-add-dir "emacs-private"))
(unless (require 'misc nil t)
    (message "Unable to load misc."))
