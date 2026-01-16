;;; setup-lsp.el --- LSP and Pyright configuration
;;; =============================================

(require 'lsp-mode)
(require 'lsp-pyright)

;; LSP configuration
(setq lsp-diagnostic-package :flycheck)
(setq lsp-enable-snippet t)
(setq lsp-enable-suggest-server-download nil)

;; Pyright configuration
(setq lsp-pyright-python-executable-cmd
      "/home/adnan/anaconda3/bin/python")
(setq lsp-pyright-type-checking-mode "off")

;; Enable LSP for regular Python files
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-ts-mode-hook #'lsp-deferred)

;; Keybindings for LSP
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
(define-key lsp-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key lsp-mode-map (kbd "M-,") 'lsp-find-references)
(define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)
(define-key lsp-mode-map (kbd "C-c C-f") 'lsp-format-buffer)

;; Auto-start LSP servers
(setq lsp-auto-guess-root t)
(setq lsp-restart 'auto-restart)

;; Enable file watching
(setq lsp-enable-file-watchers t)

;; Performance settings
(setq lsp-idle-delay 0.5)
(setq lsp-log-io nil)  ; Disable logging unless debugging

(message "✓ LSP configured for Python files")

(provide 'setup-lsp)
;;; setup-lsp.el ends here
