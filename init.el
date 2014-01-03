(setq default-buffer-file-coding-system 'utf-8)

(when (eq system-type 'windows-nt) (set-frame-font "Consolas"))
(when (eq system-type 'gnu/linux)
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(defconst emacs-add-dir "~/emacs-add")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
  (require 'mozc)
  (setq default-input-method "japanese-mozc"))

;; ----- unbinded keys -----
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(setq inhibit-startup-message t)

;; ----- frames, windows -----
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq default-frame-alist
      (append '((top . 0)
                (width . 120)
                (height . 57)
                (background-color . "#F4F5F9"))))

(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)

;; ----- display, faces -----
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
                'help-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (truncate-lines-off))))

;; line number mode off
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (linum-mode-off))))

(set-face-attribute 'region nil :background "SteelBlue1")

(global-set-key
 (kbd "C-x C-3")
 (lambda (&optional n)
   (interactive "P")
   (let ((n1 (if (null n) 2 (- n 1))))
     (loop repeat n1
           do (progn
                (split-window-horizontally)
                (balance-windows-area))))))

;; ----- interpreter -----
(global-set-key (kbd "C-/ C-i") 'ielm)

;; ----- project, directories -----
(add-to-list 'load-path (concat emacs-add-dir "/direx-el"))
(require 'direx)

(defun in-git-repository-p ()
  (string=
   (replace-regexp-in-string
    "[\n\r]+$" ""
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-repository-root ()
  (if (in-git-repository-p)
      (replace-regexp-in-string
       "[\n\r]+$" ""
       (shell-command-to-string "git rev-parse --show-toplevel"))
    nil))

(defun direx:jump-to-git-repository ()
  (interactive)
  (let ((git-repo (git-repository-root)))
    (if git-repo (direx:find-directory git-repo)
      (display-message-or-buffer "Couldn't find git repository."))))

(global-set-key (kbd "C-/ j g") 'direx:jump-to-git-repository)
(global-set-key (kbd "C-/ j d") 'direx:find-directory)

(add-to-list 'load-path (concat emacs-add-dir "/git-modes"))
(add-to-list 'load-path (concat emacs-add-dir "/magit"))
(require 'magit)

;; ----- edit mode, editing support -----
;;  https://code.google.com/p/google-styleguide/
(add-to-list 'load-path (concat emacs-add-dir "/google-style-el"))
(require 'google-c-style)
(add-hook 'c-mode-common-hook
          (lambda (&optional opt)
            (google-set-c-style)))

;; auto-complete - https://github.com/auto-complete/auto-complete
;; auto-complete dependes on poup-el - https://github.com/auto-complete/popup-el
(add-to-list 'load-path (concat emacs-add-dir "/popup-el"))
(add-to-list 'load-path (concat emacs-add-dir "/auto-complete"))
(require 'auto-complete)
(global-auto-complete-mode 1)

;; paredit - http://mumble.net/~campbell/emacs/
(add-to-list 'load-path (concat emacs-add-dir "/paredit"))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; unbinded keys M-<down>, M-<up>, C-M-f, C-M-b
(dolist (hooks (list
                 'emacs-lisp-mode-hook
                 'eval-expression-minibuffer-setup-hook
                 'ielm-mode-hook
                 'lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'scheme-mode-hook
                 'c-mode-hook
                 'c++-mode-hook))
  (add-hook hooks 'enable-paredit-mode))

(add-to-list 'load-path (concat emacs-add-dir "/qml-simple-mode"))
(autoload 'qml-simple-mode "qml-simple-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-simple-mode))

;; tab, indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-; C-s") 'replace-string)
(global-set-key (kbd "C-; C-r") 'replace-regexp)
(global-set-key (kbd "C-; C-q") 'query-replace)
(global-set-key (kbd "C-; C-x") 'query-replace-regexp)

(defun delete-forward-whitespace ()
  (interactive)
  (let ((n 0))
    (loop while (string-match "[ \t]" (string (char-after (point))))
          do (progn (setq n (+ n 1)) (forward-char)))
    (delete-char (- n))))

(global-set-key (kbd "C-; d") 'delete-forward-whitespace)
(global-set-key (kbd "C-; C-d") 'delete-forward-whitespace)

(defun replace-regexp-all (regexp to-string)
  (save-excursion
    (goto-char 0)
    (replace-regexp regexp to-string)))

;; Remove spaces in back of each line.
(global-set-key (kbd "C-; C-<SPC>") 'delete-trailing-whitespace)

;; Remove spaces which are input automatically
;; in front of parentheses by paredit.
(global-set-key
 (kbd "C-; C-8")
 (lambda (&optional opt)
   (interactive)
   (let* ((m (buffer-mode (current-buffer))))
     (if (or (eq m 'qml-simple-mode) (c-major-mode-is m))
         (replace-regexp-all "\\([^=]\\) +\\((\\)" "\\1\\2")
       (display-message-or-buffer "The buffer mode is not compatible.")))))

;; ----- desktop -----
(desktop-save-mode t)
(global-set-key (kbd "C-/ C-d") 'desktop-change-dir)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (setq search-ring nil)
            (setq regexp-search-ring nil)
            (setq file-name-history nil)
            (require 'private-utility)
            ))

;; ----- el utility -----
(defun buffer-mode (buffer-or-string)
  "Get major mode of paticular buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun eval-sexp-forwardn (&optional n)
  (interactive "P")
  (if (not (eq (buffer-mode (current-buffer)) 'emacs-lisp-mode))
      (display-message-or-buffer "The buffer is not elisp.")
    (let ((n1 (if (null n) 1 n)))
      (loop repeat n1
            do (let ((b (point))
                     (e (progn (forward-sexp) (point))))
                 (eval-region b e))))))

(global-set-key (kbd "C-; C-e") 'eval-sexp-forwardn)
(global-set-key (kbd "C-; e") 'eval-sexp-forwardn)

;; ----- private utility -----
(add-to-list 'load-path "~/emacs-add/private")
