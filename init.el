(when (eq system-type 'windows-nt) (set-frame-font "Consolas"))

(defconst emacs-add-dir "~/emacs-add")

;; ----- unbinded keys -----
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))
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

;; ----- display -----
(setq-default truncate-lines t)
(defun truncate-lines-off () (setq truncate-lines nil))

(setq-default column-number-mode t)

(setq-default linum-mode t)
(defun linum-mode-off () (linum-mode 0))

;; ----- shell, interpreter -----
(dolist (hooks (list
                'comint-output-filter-functions
                'ielm-mode-hook
                'eshell-mode-hook))
  (add-hook hooks (lambda (&optional opt)
                    (truncate-lines-off)
                    (linum-mode-off))))

(global-set-key (kbd "C-/ C-i") 'ielm)

;; ----- file coding system -----
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.el\\'" buffer-file-name))
              (set-buffer-file-coding-system 'undecided-unix)
              (set-buffer-modified-p nil))))


;; ----- edit mode -----
(add-to-list 'load-path (concat emacs-add-dir "/editors"))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; unbinded keys M-<down>, M-<up>
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(autoload 'qml-simple-mode "qml-simple-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-simple-mode))

;; ----- tab, indent -----
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ----- editing -----
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/ C-s") 'replace-string)
(global-set-key (kbd "C-/ C-r") 'replace-regexp)
(global-set-key (kbd "C-/ C-q") 'query-replace)
(global-set-key (kbd "C-/ C-x") 'query-replace-regexp)

;; ----- desktop -----
(desktop-save-mode t)
(global-set-key (kbd "C-/ C-d") 'desktop-change-dir)
(add-hook 'desktop-after-read-hook (lambda () (setq search-ring nil)))
