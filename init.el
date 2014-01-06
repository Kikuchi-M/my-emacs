(setq default-buffer-file-coding-system 'utf-8)

(when (eq system-type 'windows-nt) (set-frame-font "Consolas"))
(when (eq system-type 'gnu/linux)
  (set-frame-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(defconst emacs-add-dir "~/emacs-add")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(require 'cl)

(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
  (if (not (require 'mozc))
      (display-message-or-buffer "!! Unable to load mozc.")
    (setq default-input-method "japanese-mozc")))

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

(global-set-key
 (kbd "C-x C-3")
 (lambda (&optional n)
   (interactive "P")
   (let ((n1 (if (null n) 2 (- n 1))))
     (loop repeat n1
           do (progn
                (split-window-horizontally)
                (balance-windows-area))))))

(global-set-key (kbd "S-<up>") 'scroll-down-line)
(global-set-key (kbd "S-<down>") 'scroll-up-line)

(global-set-key
 (kbd "C-x w d")
 (lambda (&optional opt)
   (interactive "P")
   (set-window-dedicated-p (get-buffer-window) (if opt t nil))))

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

;; ----- interpreter -----
(global-set-key (kbd "C-/ C-i") 'ielm)

;; ----- project, directories -----
;; direx original - https://github.com/m2ym/direx-el
;;(unless (package-installed-p 'direx)
;;  (package-install 'direx))
(add-to-list 'load-path (concat emacs-add-dir "/direx-el"))
(if (not (require 'direx nil t))
    (display-message-or-buffer "!! Unable to load direx.")
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
      (if (not git-repo)
          (display-message-or-buffer "Couldn't find git repository.")
        (direx:find-directory git-repo)
        (set-window-dedicated-p (get-buffer-window) t))))

  (global-set-key (kbd "C-/ j g") 'direx:jump-to-git-repository)
  (global-set-key (kbd "C-/ j d") 'direx:find-directory))

;; magit - https://github.com/magit/magit
;; depends on git-modes - https://github.com/magit/git-modes
(add-to-list 'load-path (concat emacs-add-dir "/git-modes"))
(add-to-list 'load-path (concat emacs-add-dir "/magit"))
(if (not (require 'magit))
    (display-message-or-buffer "!! Unable to load magit."))
;;(unless (package-installed-p 'magit)
;;  (package-install 'magit))

;; ----- edit mode, editing support -----
;; google-c-style - https://code.google.com/p/google-styleguide/
(add-to-list 'load-path (concat emacs-add-dir "/google-style-el"))
(if (not (require 'google-c-style nil t))
    (display-message-or-buffer "!! Unable to load google-c-style.")
  (add-hook 'c-mode-common-hook
            (lambda (&optional opt)
              (google-set-c-style))))
;;(unless (package-installed-p 'google-c-style)
;;  (package-install 'google-c-style))

;; auto-complete - https://github.com/auto-complete/auto-complete
;; auto-complete dependes on poup-el - https://github.com/auto-complete/popup-el
(add-to-list 'load-path (concat emacs-add-dir "/popup-el"))
(add-to-list 'load-path (concat emacs-add-dir "/auto-complete"))
(if (not (require 'auto-complete nil t))
    (display-message-or-buffer "!! Unable to load auto-complete.")
  (global-auto-complete-mode 1))
;;(unless (package-installed-p 'auto-complete)
;;  (package-install 'auto-complete))

;; paredit - http://mumble.net/~campbell/git/paredit.git/
;;         - https://github.com/goncha/paredit
(add-to-list 'load-path (concat emacs-add-dir "/paredit"))
(if (not (require 'paredit nil t))
    (display-message-or-buffer "!! Unable to load paredit.")
  ;; unbinded keys M-<down>, M-<up>, C-M-<down>, C-M-<up>, C-M-f, C-M-b
  (dolist (hooks (list
                  'emacs-lisp-mode-hook
                  'eval-expression-minibuffer-setup-hook
                  'ielm-mode-hook
                  'lisp-mode-hook
                  'lisp-interaction-mode-hook
                  'scheme-mode-hook
                  'c-mode-hook
                  'c++-mode-hook))
    (add-hook hooks 'enable-paredit-mode)))

;; qml-simple-mode -
(add-to-list 'load-path (concat emacs-add-dir "/qml-simple-mode"))
(if (not (require 'qml-simple-mode nil t))
    (display-message-or-buffer "!! Unable to load qml-mode.")
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-simple-mode)))

;; tab, indent
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'tab-to-tab-stop)
(setq-default tab-width 4)

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-; C-s") 'replace-string)
(global-set-key (kbd "C-; C-r") 'replace-regexp)
(global-set-key (kbd "C-; C-q") 'query-replace)
(global-set-key (kbd "C-; C-x") 'query-replace-regexp)

(require 'cc-mode)
(global-set-key (kbd "C-; d") 'c-hungry-delete-forward)
(global-set-key (kbd "C-; C-d") 'c-hungry-delete-forward)

;; Remove spaces in back of each line.
(global-set-key (kbd "C-; C-<SPC>") 'delete-trailing-whitespace)

(defun replace-regexp-all (regexp to-string)
  (save-excursion
    (goto-char 0)
    (replace-regexp regexp to-string)))

;; Remove spaces which are input automatically
;; in front of parentheses by paredit.
(global-set-key
 (kbd "C-; C-8")
 (lambda (&optional opt)
   (interactive)
   (let* ((m (buffer-mode (current-buffer))))
     (if (or (eq m 'qml-simple-mode) (c-major-mode-is m))
         (replace-regexp-all "([^=]) +(\[|\() *([^ ]?.*[^ ]?)" "\\1\\2\\3")
       (display-message-or-buffer "The buffer mode is not compatible.")))))

;; highlighting
(global-set-key (kbd "C-x w h") 'highlight-regexp)
(global-set-key (kbd "C-x w r") 'unhighlight-regexp)

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
