;;; init.el --- Python + Pyright + Org + Jupyter + R (COMPLETE SETUP)
;;; ============================================================

;; ----------------------------
;; BASIC
;; ----------------------------
(setq inhibit-startup-message t)

;; Load theme
(load-theme 'tango-dark t)

;; Customize Org-mode faces specifically
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "LightSteelBlue" :italic nil))))
 '(font-lock-keyword-face ((t (:foreground "LightSkyBlue" :bold t))))
 '(org-block ((t (:background "#2e3436" :foreground "WhiteSmoke"))))
 '(org-block-begin-line ((t (:background "#2a2a2a" :foreground "LightGray" :slant italic))))
 '(org-block-end-line ((t (:background "#2a2a2a" :foreground "LightGray" :slant italic))))
 '(org-bold ((t (:weight bold))))
 '(org-code ((t (:foreground "WhiteSmoke" :background "#3a3a3a"))))
 '(org-document-info ((t (:foreground "LightSteelBlue"))))
 '(org-document-title ((t (:foreground "gold" :weight bold :height 1.4))))
 '(org-italic ((t (:slant italic))))
 '(org-level-1 ((t (:foreground "LightSkyBlue" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "LightGreen" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "LightSalmon" :weight bold))))
 '(org-level-4 ((t (:foreground "LightSteelBlue" :weight bold))))
 '(org-link ((t (:foreground "DeepSkyBlue" :underline t))))
 '(org-list-dt ((t (:foreground "LightSkyBlue"))))
 '(org-meta-line ((t (:foreground "DimGray" :background "#3a3a3a" :slant italic))))
 '(org-table ((t (:foreground "LightGreen" :background "#2a2a2a"))))
 '(org-underline ((t (:underline t))))
 '(org-verbatim ((t (:foreground "WhiteSmoke" :background "#3a3a3a"))))
 '(web-mode-html-tag-face ((t (:foreground "LightCoral" :weight normal)))))

;; Optional: Configure Org mode to show special elements
(setq org-hide-emphasis-markers nil)  ; Show */_/= markers
(setq org-pretty-entities t)          ; Show entities prettily
(setq org-fontify-quote-and-verse-blocks t)  ; Fontify quote blocks

;; Theme toggle function
(defun toggle-theme ()
  "Toggle between tango-dark and modus-vivendi themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'tango-dark)
      (progn
        (disable-theme 'tango-dark)
        (load-theme 'modus-vivendi t))
    (progn
      (disable-theme 'modus-vivendi)
      (load-theme 'tango-dark t))))

;; Bind F9 to toggle theme
(global-set-key (kbd "<f9>") 'toggle-theme)

;; Additional keybindings for Org mode
(global-set-key (kbd "C-c o") 'org-mode)  ; Quick org mode toggle

;; ----------------------------
;; CONDA
;; ----------------------------
(setenv "PATH" (concat "/path/anaconda3/bin:" (getenv "PATH")))
(setq exec-path (cons "/path/anaconda3/bin" exec-path))

;; ----------------------------
;; SMART PACKAGE LOADER
;; ----------------------------
(defun try-package-manager-install (package)
  "Try to install PACKAGE via package manager, return t if successful."
  (condition-case err
      (progn
        (unless (package-installed-p package)
          (package-install package))
        t)
    (error 
     (message "⚠ Could not install %s via package manager: %s" package (error-message-string err))
     nil)))

;; Initialize package system
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Try to refresh, but continue if it fails
(condition-case err
    (unless package-archive-contents
      (package-refresh-contents))
  (error 
   (message "⚠ Could not refresh package archives: %s" (error-message-string err))))

;; ----------------------------
;; PACKAGE INSTALLATION
;; ----------------------------
;; List of essential packages
(setq my-essential-packages '(company lsp-mode lsp-pyright flycheck 
                                      anaconda-mode company-anaconda
                                      yasnippet company-quickhelp
                                      emacs-jupyter ess))  ; Added ESS for R

;; Try package manager installation
(dolist (pkg my-essential-packages)
  (unless (package-installed-p pkg)
    (condition-case err
        (progn
          (package-install pkg)
          (message "✓ Installed %s" pkg))
      (error
       (message "⚠ Failed to install %s: %s" pkg (error-message-string err))))))

;; ----------------------------
;; LOAD PACKAGES
;; ----------------------------
(require 'company)
(require 'lsp-mode)
(require 'lsp-pyright)
(require 'flycheck)
(require 'anaconda-mode)
(require 'company-anaconda)
(require 'yasnippet)
(when (featurep 'company-quickhelp)
  (require 'company-quickhelp))

;; Load jupyter if available
(when (featurep 'jupyter)
  (require 'jupyter))

;; Load ESS for R - FORCE LOAD WITH ALL FEATURES
(when (featurep 'ess)
  (require 'ess)
  ;; Load ESS extensions
  (require 'ess-site)  ; This loads all ESS modes including ess-r-mode
  (require 'ess-r-mode) ; Explicitly load R mode
  (require 'ob-R))  ; For R support in org-babel

(require 'org)
(require 'ob-python)

;; ----------------------------
;; ENHANCED COMPANY SETTINGS (GLOBAL)
;; ----------------------------
;; Faster and better completion
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

;; Enable global modes
(global-company-mode 1)
(global-flycheck-mode 1)
(yas-global-mode 1)

;; Enable company-quickhelp if available
(when (featurep 'company-quickhelp)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.5))

;; Enhanced company backends - SIMPLIFIED
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

(message "✅ Enhanced Company & Flycheck activated")

;; ----------------------------
;; LSP FOR REGULAR .PY FILES
;; ----------------------------
(setq lsp-diagnostic-package :flycheck)
(setq lsp-enable-snippet t)
(setq lsp-enable-suggest-server-download nil)

(setq lsp-pyright-python-executable-cmd
      "/path/anaconda3/bin/python")
(setq lsp-pyright-type-checking-mode "off")
(message "✓ LSP Pyright configured")

;; Regular .py files: Use LSP
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-ts-mode-hook #'lsp-deferred)
(message "✓ LSP configured for regular Python files")

;; ----------------------------
;; ANACONDA SETUP (For Python completion)
;; ----------------------------
;; Configure anaconda to use your Anaconda Python
(setq anaconda-mode-installation-directory "/path/anaconda3/")
(setq python-shell-interpreter "/path/anaconda3/bin/python")
(message "✓ Anaconda configured")

;; ----------------------------
;; ESS FOR REGULAR .R FILES (like LSP for Python)
;; ----------------------------
(when (featurep 'ess)
  ;; Basic ESS setup for .R files
  (setq inferior-ess-r-program "/path/anaconda3/bin/R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ess-eval-visibly-p nil)
  (setq ess-history-file nil)
  
  ;; Disable flymake (causing lintr error)
  (setq ess-use-flymake nil)
  
  ;; Smart underscore (_ becomes <-)
  (setq ess-smart-S-assign-key t)
  (setq ess-S-assign-key "_")
  
  ;; Syntax highlighting
  (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:%op% . t)))
  
  ;; Setup for regular .R files (FULL FEATURES)
  (defun my/ess-r-full-setup ()
    "Full ESS setup for regular .R files (like LSP for Python)."
    (when (eq ess-dialect "R")
      (message "🎯 Setting up full ESS for .R file")
      
      ;; Enable company with ESS backends
      (company-mode 1)
      (yas-minor-mode 1)
      
      ;; FULL ESS BACKENDS for .R files
      (setq-local company-backends
                  '(company-R-args      ; R function arguments
                    company-R-objects   ; R objects
                    company-yasnippet
                    company-keywords
                    company-files
                    company-dabbrev-code
                    company-dabbrev))
      
      ;; Configure company
      (setq-local company-idle-delay 0.3)
      (setq-local company-minimum-prefix-length 2)
      (setq-local company-tooltip-limit 20)
      
      ;; Disable flymake if it's causing issues
      (when (bound-and-true-p flymake-mode)
        (flymake-mode -1))
      
      (message "✅ Full ESS setup for .R file (like LSP for Python)")))
  
  ;; Hook for regular .R files
  (add-hook 'ess-mode-hook 'my/ess-r-full-setup)
  
  (message "✓ ESS configured for regular .R files (like LSP for Python)"))

;; ----------------------------
;; JUPYTER SETUP (MINIMAL & GUARANTEED)
;; ----------------------------
;; First, ensure the package is installed and loaded
(unless (package-installed-p 'jupyter)
  (package-refresh-contents)
  (package-install 'jupyter))

;; Now require it (force load)
(require 'jupyter)

;; Configure for your Anaconda
(setq jupyter-python-command "/path/anaconda3/bin/python")
(setq jupyter-server-command "/path/anaconda3/bin/jupyter")

;; Tell Org-mode about Jupyter
(org-babel-do-load-languages
 'org-babel-load-languages
 '((jupyter . t)
   (python . t)
   (R . t)))

;; Optional: Set default parameters
(setq org-babel-default-header-args:jupyter-python
      '((:kernel . "python3")
        (:session . "default")
        (:async . "yes")
        (:results . "output drawer replace")
        (:exports . "both")
        (:cache . "no")))

;; Add default parameters for Jupyter R
(setq org-babel-default-header-args:jupyter-R
      '((:kernel . "ir")  ; IR kernel name
        (:session . "default")
        (:async . "yes")
        (:results . "output drawer replace")
        (:exports . "both")
        (:cache . "no")))

(message "✅ Jupyter configured and ready for #+BEGIN_SRC jupyter-python and jupyter-R")

;; ----------------------------
;; ORG MODE CONFIGURATION (ENHANCED)
;; ----------------------------
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

;; 🔥 WINDOW SETUP - Choose your preferred layout:
;; 'current-window     - Replace current window
;; 'split-window-below - Split horizontally, source below (RECOMMENDED)
;; 'split-window-right - Split vertically, source on right
(setq org-src-window-setup 'split-window-below)

(setq org-src-use-current-buffer t)
(setq org-edit-src-content-indentation 0)

;; Set language modes - FIXED: Use 'ess-r' not 'ess-r-mode'
(setq org-src-lang-modes '(("python" . python)
                           ("jupyter-python" . python)
                           ("R" . ess-r)          ; ← FIXED HERE
                           ("r" . ess-r)          ; ← FIXED HERE
                           ("jupyter-R" . ess-r))) ; ← FIXED HERE

;; Load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (jupyter . t)))

;; 🔥 CRITICAL: Enable automatic inline image display
(setq org-display-inline-images t)      ; Show images inline
(setq org-redisplay-inline-images t)    ; Auto-refresh images
(setq org-startup-with-inline-images t) ; Show on startup

;; Optional: Control image size
(setq org-image-actual-width nil)       ; Show at actual size

;; ----------------------------
;; ORG-MODE SPECIFIC SETUP (ENHANCED ANACONDA FOR ORG PYTHON)
;; ----------------------------
(defun my/org-python-completion-setup ()
  "Setup enhanced Anaconda completion for Org-mode Python source blocks."
  (when (and (derived-mode-p 'python-mode)
             ;; Check if we're in an Org source buffer
             (or (string-match-p "\\*Org SRC" (buffer-name))
                 (string-match-p "org-src-" (buffer-name))
                 (bound-and-true-p org-src-mode)))
    
    (message "🎯 Setting up ENHANCED completion for Org Python")
    
    ;; Enable anaconda-mode with all features
    (anaconda-mode 1)
    (anaconda-eldoc-mode 1)
    
    ;; Enhanced company settings for Org (even faster)
    (setq-local company-idle-delay 0.05)         ; Super fast in Org
    (setq-local company-minimum-prefix-length 1) ; Start immediately
    (setq-local company-tooltip-limit 20)        ; More suggestions
    
    ;; Special backend order for Org
    (setq-local company-backends
                '(company-anaconda
                  company-yasnippet
                  company-capf
                  company-keywords
                  company-files
                  company-dabbrev-code
                  company-dabbrev))
    
    ;; Enable snippet expansion
    (yas-minor-mode 1)
    
    ;; Quick help for documentation
    (when (featurep 'company-quickhelp)
      (setq-local company-quickhelp-delay 0.3))
    
    (message "✅ ENHANCED completion activated in Org source block")))

;; Hook to run when entering Org source edit mode
(add-hook 'org-src-mode-hook 'my/org-python-completion-setup)

;; Also add to python-mode-hook for safety
(add-hook 'python-mode-hook
          (lambda ()
            (when (or (string-match-p "\\*Org SRC" (buffer-name))
                      (string-match-p "org-src-" (buffer-name)))
              (my/org-python-completion-setup))))



;;......................
;; github
;;.....................

;; --- GitHub Workflow Setup ---

;; 1. Markdown mode (you have it!)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Optional: Markdown commands
(setq markdown-command "pandoc")  ; For export if you have pandoc

;; 2. Grip mode (you have it!)
(require 'grip-mode)
;; Auto-start grip for README files
(add-hook 'markdown-mode-hook
          (lambda ()
            (when (string-match "README" (buffer-name))
              (grip-mode))))

;; 3. Magit (you have it!)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; 4. Git link
(require 'git-link)
(global-set-key (kbd "C-c g l") 'git-link)










;; ----------------------------
;; R COMPLETION FOR ORG-MODE BLOCKS (ANACONDA-STYLE FOR R)
;; ----------------------------
(defun my/org-r-completion-setup ()
  "Setup simple R completion for Org-mode source blocks (ANACONDA-STYLE FOR R)."
  (when (and (derived-mode-p 'ess-r-mode)
             ;; Check if we're in an Org source buffer
             (or (string-match-p "\\*Org SRC" (buffer-name))
                 (string-match-p "org-src-" (buffer-name))
                 (bound-and-true-p org-src-mode)))
    
    (message "🎯 Setting up SIMPLE R completion for Org block (Anaconda-style)")
    
    ;; SIMPLE BACKENDS FOR ORG MODE - just like Anaconda for Python
    (setq-local company-backends
                '(company-keywords      ; R keywords only (FAST & RELIABLE)
                  company-files         ; File names
                  company-dabbrev-code  ; Code from buffers
                  company-dabbrev))     ; Text from buffers
    
    ;; Faster completion in Org
    (setq-local company-idle-delay 0.05)         ; Super fast
    (setq-local company-minimum-prefix-length 1) ; Start immediately
    (setq-local company-tooltip-limit 10)        ; Fewer suggestions
    
    ;; Enable company
    (company-mode 1)
    (yas-minor-mode 1)
    
    ;; Force syntax highlighting refresh
    (font-lock-flush)
    (font-lock-ensure)
    
    ;; Disable flymake if enabled (causes lintr errors in org)
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    
    (message "✅ SIMPLE R completion activated in Org (like Anaconda for Python)")))

;; Hook for ESS R mode in org source blocks
(add-hook 'org-src-mode-hook
          (lambda ()
            (when (derived-mode-p 'ess-r-mode)
              (my/org-r-completion-setup))))

;; ----------------------------
;; FIX FOR ESS-R MODE DETECTION
;; ----------------------------
(defun my/check-ess-r-mode ()
  "Debug function to check if ess-r-mode is available."
  (interactive)
  (message "Checking ESS...")
  (message "ESS feature: %s" (featurep 'ess))
  (message "ESS R mode available: %s" (fboundp 'ess-r-mode))
  (message "Current major mode: %s" major-mode))

;; ----------------------------
;; HELPER FUNCTIONS FOR CODE BLOCKS
;; ----------------------------
(defun my/insert-jupyter-python-block ()
  "Insert a jupyter-python source block."
  (interactive)
  (if (featurep 'jupyter)
      (progn
        (insert "#+BEGIN_SRC jupyter-python\n\n#+END_SRC")
        (backward-char 10)
        (message "✓ Jupyter Python block inserted (for plots/images)"))
    (message "⚠ Jupyter not available - installing emacs-jupyter package")))

(defun my/insert-regular-python-block ()
  "Insert a regular python source block."
  (interactive)
  (insert "#+BEGIN_SRC python\n\n#+END_SRC")
  (backward-char 10)
  (message "✓ Regular Python block inserted"))

(defun my/insert-r-block ()
  "Insert an R source block."
  (interactive)
  (insert "#+BEGIN_SRC R\n\n#+END_SRC")
  (backward-char 9)
  (message "✓ R block inserted"))

(defun my/insert-jupyter-r-block ()
  "Insert a jupyter-R source block."
  (interactive)
  (if (featurep 'jupyter)
      (progn
        (insert "#+BEGIN_SRC jupyter-R\n\n#+END_SRC")
        (backward-char 10)
        (message "✓ Jupyter R block inserted (for plots/images)"))
    (message "⚠ Jupyter not available")))

;; ----------------------------
;; QUICK ACCESS KEYBINDINGS
;; ----------------------------
(define-key org-mode-map (kbd "C-c j") 'my/insert-jupyter-python-block)
(define-key org-mode-map (kbd "C-c p") 'my/insert-regular-python-block)
(define-key org-mode-map (kbd "C-c r") 'my/insert-r-block)
(define-key org-mode-map (kbd "C-c R") 'my/insert-jupyter-r-block)
(define-key org-mode-map (kbd "C-c t") 'my/test-org-python-completion)
(define-key org-mode-map (kbd "C-c T") 'my/test-r-completion-org)  ; New: R completion test
(define-key org-mode-map (kbd "C-c d") 'my/check-ess-r-mode)  ; Debug key
(define-key python-mode-map (kbd "C-c TAB") 'company-complete)
(define-key org-src-mode-map (kbd "C-c TAB") 'company-complete)

;; Jupyter specific keybindings (if available)
(when (featurep 'jupyter)
  (define-key jupyter-repl-mode-map (kbd "C-c C-c") 'jupyter-eval-line-or-region)
  (define-key jupyter-repl-mode-map (kbd "C-RET") 'jupyter-eval-line-or-region))

;; ----------------------------
;; TEST FUNCTIONS
;; ----------------------------
(defun my/test-org-python-completion ()
  "Test Python completion in Org mode."
  (interactive)
  (let ((test-buffer (get-buffer-create "*Org Python Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "#+BEGIN_SRC python\nimport numpy as np\n\ndef test():\n    return 'Hello'\n#+END_SRC")
      (org-mode)
      (goto-char 20)  ; Move inside source block
      (org-edit-special)
      (message "Test: Entered Python source block in Org")
      (sit-for 0.5)
      (message "Mode: %s, Anaconda: %s, Yasnippet: %s" 
               major-mode
               (bound-and-true-p anaconda-mode)
               (bound-and-true-p yas-minor-mode)))))

(defun my/test-r-completion-org ()
  "Test R completion in Org mode (Anaconda-style)."
  (interactive)
  (let ((test-buffer (get-buffer-create "*Org R Completion Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (org-mode)
      (insert "* R Completion Test (Org Mode - Anaconda-style)\n\n")
      (insert "#+BEGIN_SRC R\n")
      (insert "# Test SIMPLE completion - type slowly:\n")
      (insert "# 1. Type 'pr' - should suggest print\n")
      (insert "# 2. Type 'me' - should suggest mean\n")
      (insert "# 3. Type 'su' - should suggest sum\n")
      (insert "# 4. Type 'mtc' - should suggest mtcars\n\n")
      (insert "# Keywords completion only (fast & reliable)\n")
      (insert "print('test')\n")
      (insert "mean(c(1,2,3))\n")
      (insert "sum(1:10)\n")
      (insert "mtcars\n")
      (insert "#+END_SRC\n")
      (switch-to-buffer test-buffer)
      (goto-char (point-min))
      (message "✓ R completion test created - edit with C-c ' (Anaconda-style)"))))

(defun my/test-jupyter-output ()
  "Test Jupyter rich output in Org."
  (interactive)
  (when (featurep 'jupyter)
    (let ((test-buffer (get-buffer-create "*Jupyter Test*")))
      (with-current-buffer test-buffer
        (erase-buffer)
        (org-mode)
        (insert "* Jupyter Output Test\n\n")
        (insert "#+BEGIN_SRC jupyter-python\n")
        (insert "import matplotlib.pyplot as plt\n")
        (insert "import numpy as np\n\n")
        (insert "x = np.linspace(0, 10, 100)\n")
        (insert "y = np.sin(x)\n")
        (insert "plt.plot(x, y)\n")
        (insert "plt.title('Sine Wave')\n")
        (insert "plt.show()\n")
        (insert "#+END_SRC\n\n")
        (insert "Execute with: C-c C-c on the block")
        (switch-to-buffer test-buffer)
        (message "✓ Jupyter test block created - execute with C-c C-c")))))

(defun my/test-python-plot ()
  "Test Python plotting that works in regular python blocks."
  (interactive)
  (let ((test-buffer (get-buffer-create "*Python Plot Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (org-mode)
      (insert "* Python Plot Test (Guaranteed Working)\n\n")
      (insert "This uses regular python block with file output:\n\n")
      (insert "#+BEGIN_SRC python :results output :exports results\n")
      (insert "import matplotlib\n")
      (insert "matplotlib.use('Agg')  # Non-interactive backend\n")
      (insert "import matplotlib.pyplot as plt\n")
      (insert "import numpy as np\n")
      (insert "import os\n\n")
      (insert "# Create plot\n")
      (insert "x = np.linspace(0, 10, 100)\n")
      (insert "y = np.sin(x)\n\n")
      (insert "plt.figure(figsize=(8, 4))\n")
      (insert "plt.plot(x, y, 'b-', linewidth=2)\n")
      (insert "plt.title('Sine Wave Test')\n")
      (insert "plt.xlabel('x')\n")
      (insert "plt.ylabel('sin(x)')\n")
      (insert "plt.grid(True, alpha=0.3)\n\n")
      (insert "# Save to file\n")
      (insert "output_file = '/tmp/emacs_test_plot.png'\n")
      (insert "plt.savefig(output_file, dpi=120, bbox_inches='tight')\n")
      (insert "plt.close()\n\n")
      (insert "# Tell Org to display the image\n")
      (insert "print(f'[[file:{output_file}]]')\n")
      (insert "print('✓ Plot saved. Press C-c C-x C-v to view image inline.')\n")
      (insert "#+END_SRC\n\n")
      (insert "Instructions:\n")
      (insert "1. Put cursor in the code block\n")
      (insert "2. Press C-c C-c to execute\n")
      (insert "3. Press C-c C-x C-v to toggle image display\n")
      (switch-to-buffer test-buffer)
      (goto-char (point-min)))))

(defun my/test-r-plot ()
  "Test R plotting in Org."
  (interactive)
  (let ((test-buffer (get-buffer-create "*R Plot Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (org-mode)
      (insert "* R Plot Test\n\n")
      (insert "This uses regular R block with base graphics:\n\n")
      (insert "#+BEGIN_SRC R :results output graphics :file /tmp/r-test-plot.png\n")
      (insert "# Create a simple plot\n")
      (insert "x <- 1:10\n")
      (insert "y <- x^2\n")
      (insert "plot(x, y, type='o', col='blue', lwd=2, main='R Test Plot')\n")
      (insert "grid()\n")
      (insert "#+END_SRC\n\n")
      (insert "Execute with: C-c C-c on the block\n")
      (insert "Then C-c C-x C-v to view image\n")
      (switch-to-buffer test-buffer)
      (goto-char (point-min))
      (message "✓ R test block created - execute with C-c C-c"))))

(defun my/test-r-syntax ()
  "Test R syntax highlighting in Org."
  (interactive)
  (let ((test-buffer (get-buffer-create "*R Syntax Test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (org-mode)
      (insert "* R Syntax Highlighting Test\n\n")
      (insert "Test 1: Regular R block\n")
      (insert "#+BEGIN_SRC R\n")
      (insert "# This is a comment - should be colored\n")
      (insert "library(ggplot2)  # Load ggplot2\n")
      (insert "data <- mtcars  # Assign data\n")
      (insert "mean_mpg <- mean(data$mpg)  # Calculate mean\n")
      (insert "print(paste('Mean MPG:', mean_mpg))\n")
      (insert "plot(data$mpg, data$hp)  # Create plot\n")
      (insert "#+END_SRC\n\n")
      (insert "Test 2: Jupyter R block\n")
      (insert "#+BEGIN_SRC jupyter-R\n")
      (insert "library(tidyverse)\n")
      (insert "mtcars %>% \n")
      (insert "  filter(mpg > 20) %>%\n")
      (insert "  ggplot(aes(x=mpg, y=hp)) + geom_point()\n")
      (insert "#+END_SRC\n")
      (switch-to-buffer test-buffer)
      (goto-char (point-min))
      (message "✓ R syntax test created - try C-c ' on each block"))))

;; ----------------------------
;; FINAL STATUS REPORT
;; ----------------------------
(defun my/show-setup-status ()
  "Show current setup status."
  (interactive)
  (let ((buffer (get-buffer-create "*Emacs Setup Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== EMACS PYTHON & R DEVELOPMENT SETUP ===\n\n")
      
      (insert "🎯 CORE FEATURES:\n")
      (insert "✓ LSP/Pyright for .py files\n")
      (insert "✓ Anaconda for Org Python blocks\n")
      (insert "✓ ESS for .R files (full features)\n")
      (insert "✓ Simple completion for Org R blocks (Anaconda-style)\n")
      (insert "✓ Fast completion (0.1s delay)\n")
      (insert "✓ Snippet expansion (yasnippet)\n")
      (insert "✓ Split window editing\n")
      
      (when (featurep 'jupyter)
        (insert "✓ Jupyter for rich outputs (plots/images)\n"))
      
      (insert "\n🎯 COMPLETION STRATEGY:\n")
      (insert "Python .py files    → LSP/Pyright (full features)\n")
      (insert "Python Org blocks   → Anaconda (simple but fast)\n")
      (insert "R .R files         → ESS (full features)\n")
      (insert "R Org blocks       → Keywords only (fast & reliable)\n")
      
      (insert "\n🎯 KEYBINDINGS:\n")
      (insert "C-c '      - Edit source block\n")
      (insert "C-c j      - Insert Jupyter Python block\n")
      (insert "C-c p      - Insert regular Python block\n")
      (insert "C-c r      - Insert R block\n")
      (insert "C-c R      - Insert Jupyter R block\n")
      (insert "C-c t      - Test Python completion\n")
      (insert "C-c T      - Test R completion (org mode)\n")
      (insert "C-c d      - Debug ESS\n")
      (insert "C-c TAB    - Force completion\n")
      
      (insert "\n🎯 ORG MODE USAGE:\n")
      (insert "#+BEGIN_SRC python        - Regular Python (Anaconda completion)\n")
      (insert "#+BEGIN_SRC R             - Regular R (simple keyword completion)\n")
      
      (when (featurep 'jupyter)
        (insert "#+BEGIN_SRC jupyter-python - Jupyter Python (plots/images)\n")
        (insert "#+BEGIN_SRC jupyter-R      - Jupyter R (plots/images)\n"))
      
      (insert "\n=== PACKAGE STATUS ===\n")
      (dolist (pkg '(company lsp-mode lsp-pyright flycheck anaconda-mode 
                     company-anaconda yasnippet company-quickhelp emacs-jupyter ess))
        (insert (format "%-20s %s\n" pkg 
                        (if (featurep pkg) "✓ LOADED" "✗ MISSING"))))
      
      (insert "\n=== TEST COMMANDS ===\n")
      (insert "M-x my-test-org-python-completion  - Test Python completion\n")
      (insert "M-x my-test-r-completion-org       - Test R completion (org mode)\n")
      (insert "M-x my-test-python-plot            - Test Python plotting\n")
      (insert "M-x my-test-r-plot                 - Test R plotting\n")
      (insert "M-x my-test-r-syntax               - Test R syntax highlighting\n")
      (insert "M-x my-check-ess-r-mode            - Debug ESS R mode\n")
      (insert "M-x my-show-setup-status           - This screen\n")
      
      (switch-to-buffer buffer)
      (special-mode))))

;; ----------------------------
;; FINAL INIT MESSAGE
;; ----------------------------
(message "========================================")
(message "🚀 PYTHON & R DEVELOPMENT ENVIRONMENT READY")
(message "========================================")
(message "📦 Core Features:")
(message "  • LSP/Pyright for .py files")
(message "  • Anaconda for Org Python blocks") 
(message "  • ESS for .R files (full features)")
(message "  • Simple keyword completion for Org R blocks (fast & reliable)")
(message "  • Fast completion (0.1s delay, 1 char)")
(message "  • Snippet support (type 'for' + TAB)")
(message "  • Split window editing")
(when (featurep 'jupyter)
  (message "  • Jupyter for plots/images in Org"))
(message "========================================")
(message "🔧 Run M-x my-show-setup-status for details")
(message "🔧 Run M-x my-test-r-completion-org to test R completion in org")
(message "🔧 Run M-x my-check-ess-r-mode to debug ESS")
(message "========================================")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

;;; init.el ends here
