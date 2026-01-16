;;; setup-jupyter.el --- Jupyter integration
;;; =========================================

(require 'jupyter)

;; Configure for Anaconda
(setq jupyter-python-command "/home/adnan/anaconda3/bin/python")
(setq jupyter-server-command "/home/adnan/anaconda3/bin/jupyter")

;; Tell Org-mode about Jupyter
(org-babel-do-load-languages
 'org-babel-load-languages
 '((jupyter . t)))

;; Default parameters for Jupyter blocks
(setq org-babel-default-header-args:jupyter-python
      '((:kernel . "python3")
        (:session . "default")
        (:async . "yes")
        (:results . "output drawer replace")
        (:exports . "both")
        (:cache . "no")))

(setq org-babel-default-header-args:jupyter-R
      '((:kernel . "ir")  ; IR kernel name
        (:session . "default")
        (:async . "yes")
        (:results . "output drawer replace")
        (:exports . "both")
        (:cache . "no")))

;; Jupyter keybindings
(define-key jupyter-repl-mode-map (kbd "C-c C-c") 'jupyter-eval-line-or-region)
(define-key jupyter-repl-mode-map (kbd "C-RET") 'jupyter-eval-line-or-region)

;; Helper functions
(defun jupyter-insert-python-block ()
  "Insert a jupyter-python source block."
  (interactive)
  (insert "#+BEGIN_SRC jupyter-python\n\n#+END_SRC")
  (backward-char 10)
  (message "✓ Jupyter Python block inserted"))

(defun jupyter-insert-r-block ()
  "Insert a jupyter-R source block."
  (interactive)
  (insert "#+BEGIN_SRC jupyter-R\n\n#+END_SRC")
  (backward-char 10)
  (message "✓ Jupyter R block inserted"))

;; Bind to global keys
(global-set-key (kbd "C-c j p") 'jupyter-insert-python-block)
(global-set-key (kbd "C-c j r") 'jupyter-insert-r-block)

(message "✅ Jupyter configured and ready")

(provide 'setup-jupyter)
;;; setup-jupyter.el ends here
