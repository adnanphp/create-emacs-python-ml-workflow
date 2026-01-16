;;; setup-company.el --- Company mode configuration
;;; ==============================================

(require 'company)

;; Enhanced Company settings (GLOBAL)
(setq company-idle-delay 0.1)           ; Start completion faster
(setq company-minimum-prefix-length 1)  ; Start after 1 char
(setq company-selection-wrap-around t)  ; Wrap around selections
(setq company-show-numbers t)           ; Show numbers for quick selection
(setq company-tooltip-limit 15)         ; Show more candidates
(setq company-dabbrev-ignore-case nil)  ; Case-sensitive
(setq company-dabbrev-downcase nil)
(setq company-transformers '(company-sort-by-backend-importance))

;; Better UI
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

;; Company frontends
(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-preview-if-just-one-frontend
        company-echo-metadata-frontend))

;; Enable global company mode
(global-company-mode 1)

;; Enhanced company backends
(with-eval-after-load 'company
  (setq company-backends
        '((company-anaconda          ; Python from Anaconda
           company-capf              ; Completion-at-point
           company-yasnippet         ; Snippet expansion
           company-keywords          ; Language keywords
           company-files             ; File paths
           company-dabbrev-code      ; Code from buffers
           company-dabbrev           ; Text from buffers
           company-ispell)           ; Spell checking
          )))

;; Better documentation
(setq eldoc-echo-area-use-multiline-p t)
(setq eldoc-idle-delay 0.3)

;; Enable company-quickhelp if available
(when (featurep 'company-quickhelp)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.5))

;; Keybindings
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<return>") 'company-complete-selection)
(define-key company-active-map (kbd "C-g") 'company-abort)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(provide 'setup-company)
;;; setup-company.el ends here
