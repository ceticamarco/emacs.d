;; Emacs configuration
;; Marco Cetica (c) 2024 <email@marcocetica.com>
;;

;; Disable tool/status bar
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0))

;; Disable splash screen
(setq inhibit-startup-screen 1)

;; Disable ALL backup files(#,~)
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Use spaces for tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; org-agenda keymaps
(require 'org)
(define-key global-map (kbd "C-c c l") 'org-store-link)
(define-key global-map (kbd "C-c c a") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/agenda/scheduler.org"))

;; Evaluate code blocks within Org mode
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t) (shell . t) (C . t) (haskell . t)))

;; Evaluate code blocks without prompting
(setq org-confirm-babel-evaluate nil)

;; Set initial window size
(add-to-list 'default-frame-alist '(width . 154))
(add-to-list 'default-frame-alist '(height . 34))

;; Highlight matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode)

;; Add customization to 'custom.el' file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Enable MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; PACKAGE SECTION
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright (lsp-deferred)))))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . interactive-haskell-mode)
  :custom (haskell-process-show-debug-tips nil))

(use-package company
  :ensure t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package modus-themes
  :ensure t)

(use-package magit
  :ensure t)
;; END PACKAGE SECTION

;; Set custom theme
(load-theme 'modus-vivendi-deuteranopia t)

;; Custom templates
(defun my-insert-snippet-template (lang evaluate-p)
  "Insert an Org mode codeblock"
  (interactive (list (read-string "Language: ")
                     (y-or-n-p "Evaluate? ")))
  (insert (format "#+BEGIN_SRC %s :results output" lang))
  (if (equal "python" lang) (insert " :python /usr/bin/python3"))
  (if evaluate-p (insert " :eval yes\n") (insert " :eval no\n"))
  (insert "#+END_SRC\n"))

;; Custom keyboard shortcuts
(define-key global-map (kbd "C-c t s") 'my-insert-snippet-template)

