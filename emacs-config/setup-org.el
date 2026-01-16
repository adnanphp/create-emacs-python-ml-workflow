;;; setup-org.el --- Org-mode enhancements
;;; =======================================

(require 'org)
(require 'ob-python)

;; Org-mode configuration
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup 'split-window-below)
(setq org-src-use-current-buffer t)
(setq org-edit-src-content-indentation 0)

;; Set language modes
(setq org-src-lang-modes '(("python" . python)
                           ("jupyter-python" . python)
                           ("R" . ess-r)
                           ("r" . ess-r)
                           ("jupyter-R" . ess-r)))

;; Load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (jupyter . t)))

;; Image display settings
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

;; Org mode keybindings
(define-key org-mode-map (kbd "C-c '") 'org-edit-special)
(define-key org-mode-map (kbd "C-c C-c") 'org-babel-execute-src-block)
(define-key org-mode-map (kbd "C-c C-v b") 'org-babel-execute-buffer)
(define-key org-mode-map (kbd "C-c C-v v") 'org-babel-expand-src-block)
(define-key org-mode-map (kbd "C-c C-x C-v") 'org-toggle-inline-images)

;; Custom faces for Org mode
(custom-set-faces
 '(org-block ((t (:background "#2e3436" :foreground "WhiteSmoke"))))
 '(org-block-begin-line ((t (:background "#2a2a2a" :foreground "LightGray" :slant italic))))
 '(org-block-end-line ((t (:background "#2a2a2a" :foreground "LightGray" :slant italic))))
 '(org-document-title ((t (:foreground "gold" :weight bold :height 1.4))))
 '(org-level-1 ((t (:foreground "LightSkyBlue" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "LightGreen" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "LightSalmon" :weight bold))))
 '(org-link ((t (:foreground "DeepSkyBlue" :underline t)))))

;; Org mode helper functions
(defun org-insert-python-block ()
  "Insert a Python source block."
  (interactive)
  (insert "#+BEGIN_SRC python\n\n#+END_SRC")
  (backward-char 10)
  (message "✓ Python block inserted"))

(defun org-insert-r-block ()
  "Insert an R source block."
  (interactive)
  (insert "#+BEGIN_SRC R\n\n#+END_SRC")
  (backward-char 9)
  (message "✓ R block inserted"))

;; Bind helper functions
(define-key org-mode-map (kbd "C-c p") 'org-insert-python-block)
(define-key org-mode-map (kbd "C-c r") 'org-insert-r-block)

(message "✓ Org-mode configuration loaded")

(provide 'setup-org)
;;; setup-org.el ends here
