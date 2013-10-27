(defconst emacs-add-dir "~/emacs-add")

;; ----- unbinded keys -----
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))
(setq inhibit-startup-message t)

;; ----- frames, windows -----
(tool-bar-mode 0)
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
(setq-default column-number-mode t)
(dolist (linum-hook  (list
                      'emacs-lisp-mode-hook
                      'fundamental-mode-hook
                      'c-mode-hook
                      'c++-mode-hook
                      'qml-simple-mode-hook
                      ))
  (add-hook linum-hook (lambda () (linum-mode t))))
(add-hook 'find-file-hook (lambda () (linum-mode t)))

;; ----- shell, interpreter -----
(add-hook 'sh-mode-hook
          '(lambda () (setq truncate-lines nil)))
(global-set-key (kbd "C-/ C-i") 'ielm)

;; ----- file coding system -----
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.el\\'" buffer-file-name))
              (set-buffer-file-coding-system 'undecided-unix)
              (set-buffer-modified-p nil))))


;; ----- edit mode -----
;(add-to-list 'load-path (concat emacs-add-dir "/editors"))
;
;(require 'qml-simple-mode)
;(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-simple-mode))

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
