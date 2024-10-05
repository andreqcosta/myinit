;; Package repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Enable vertico
  (use-package vertico
    :ensure t
    ;;:custom
    ;; (vertico-scroll-margin 0) ;; Different scroll margin
    ;; (vertico-count 20) ;; Show more candidates
    ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
    ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    :init
    (vertico-mode))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :init
    (savehist-mode))

  ;; A few more useful configurations...
  (use-package emacs
    :custom
    ;; Support opening new minibuffers from inside existing minibuffers.
    (enable-recursive-minibuffers t)
    ;; Hide commands in M-x which do not work in the current mode.  Vertico
    ;; commands are hidden in normal buffers. This setting is useful beyond
    ;; Vertico.
    (read-extended-command-predicate #'command-completion-default-include-p)
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
		    (replace-regexp-in-string
		     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		     crm-separator)
		    (car args))
	    (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; delete word with backspace on minibuffer vertico
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  ;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(global-visual-line-mode t)

(setq-default left-margin-width 1 right-margin-width 1) ; Define new widths.
(set-window-buffer nil (current-buffer))

(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(functionp 'module-load)
(set-face-attribute 'default nil :height 100)

(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

(setq make-backup-files nil)

(setq visible-bell 1)

(setq inhibit-startup-screen t)
;; (setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(global-set-key (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "<DEL>") #'dired-up-directory)

(defun enter-above ()
  (interactive)
  (previous-line)
  (move-end-of-line 1)
  (newline 1))

(global-set-key (kbd "M-o") 'enter-above)

(defun indent-all ()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-<tab>") 'indent-all)

(defun cut-paste-in-nextline ()
  (interactive)
  (kill-ring-save (line-beginning-position 1) (line-end-position 1))
  (move-end-of-line 1)
  (newline)
  (yank 1))

(global-set-key (kbd "C-x y") 'cut-paste-in-nextline)

(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (load-theme 'zenburn t))

(use-package magit
  :ensure t)
(use-package magit-popup
  :ensure t)

(use-package company
    :ensure t
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))

    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

(global-company-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
	      ("C-o" . other-window))
  :init
  (setq vterm-buffer-name-string "vterm %s"))

(defun mvterm-below (&optional arg)
  "Split the current window 70/30 rather than 50/50.
A single-digit prefix argument gives the top window arg*10%."
  (interactive "P")
  (let ((proportion (* (or arg 7.5) 0.1)))

    (split-window-below (round (* proportion (window-height)))))
  (other-window 1)
  (vterm))
(global-set-key (kbd "C-c v") 'mvterm-below)
(global-set-key (kbd "C-c t") 'vterm)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))
