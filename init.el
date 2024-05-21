;; Emacs configuration
;; Marco Cetica (c) 2024 <email@marcocetica.com>
;;
(require 'cl-lib)

;; Disable tool/status bar
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0))

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Disable ALL backup files(#,~)
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Use spaces for tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; org-agenda keymaps
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/agenda/scuola.org" 
                             "~/Dropbox/agenda/personale.org"))

;; Evaluate code blocks within Org mode
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t) (shell . t) (C . t)))

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

;; Enable MELPA reporitory
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; PACKAGE SECTION 
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright (lsp-deferred)))))

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
(load-theme 'modus-vivendi-tritanopia t)

;; Custom templates
(defun my-read-deps ()
  (let* ((deps-list nil)
         (dep nil))
    (cl-loop
      (setq dep (read-string "Dependency: "))
      (if (string-empty-p dep)
        (cl-return deps-list)
        (push dep deps-list)))))
    
(defun my-insert-tex-template (&optional title)
  "Insert a Org mode template for LaTeX A5 documents"
  (interactive "sTitle: ")
  (insert (format "#+TITLE: %s\n" (if (equal "" title) "New Document" title)))
  (insert "#+AUTHOR: Marco Cetica\n")
  (insert "#+DATE: \\today\n")
  (insert "#+OPTIONS: geometry:left=2cm,right=2cm,top=1cm,bottom=2cm\n")
  (insert "#+PAPERSIZE: a5\n")
  (insert "#+LATEX_HEADER: \\usepackage[a5paper, left=2cm, right=2cm, top=1cm, bottom=2cm]{geometry}\n")
  (insert "#+LATEX_HEADER: \\usepackage{iwona}\n")
  (insert "#+LATEX_HEADER: \\hypersetup{colorlinks=true, linkcolor=blue, filecolor=blue, citecolor=blue, urlcolor=blue}\n")
  (insert "\\newpage\n"))

(defun my-insert-cpp-template (&optional libs)
  "Insert a C++ template with optional dependencies"
  (interactive (list (my-read-deps)))
  (cl-dolist (lib libs (if (not (null libs)) (insert "\n") nil))
             (insert (format "#include <%s>\n" lib)))
  (insert "int main(void) {\n    return 0;\n}"))

(defun my-insert-py-template (&optional libs)
  "Insert a Python template with optional imports"
  (interactive (list (my-read-deps)))
  (cl-dolist (lib libs (if (not (null libs)) (insert "\n") nil))
             (insert (format "import %s\n" lib)))
  (insert "def main():\n    print(1)\n\n")
  (insert "if __name__ == \"__main__\":\n    main()"))

(defun my-insert-snippet-template (lang evaluate-p)
  "Insert an Org mode codeblock"
  (interactive (list (read-string "Language: ")
                     (y-or-n-p "Evaluate? ")))
  (insert (format "#+BEGIN_SRC %s :results output" lang))
  (if (equal "python" lang) (insert " :python /usr/bin/python3"))
  (if evaluate-p (insert " :eval yes\n") (insert " :eval no\n"))
  (insert "#+END_SRC\n"))

;; Custom keyboard shortcuts
(global-set-key (kbd "C-c t t") 'my-insert-tex-template)
(global-set-key (kbd "C-c t c") 'my-insert-cpp-template)
(global-set-key (kbd "C-c t p") 'my-insert-py-template)
(global-set-key (kbd "C-c t s") 'my-insert-snippet-template)
