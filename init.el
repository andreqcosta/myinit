;; Package repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Configure directory extension.
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
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

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
(set-face-attribute 'default nil :height 140)

(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;;disable make of backup files
(setq make-backup-files nil)

(setq visible-bell 1)

(defun cp-line()
  (interactive)
  (beginning-of-line)
  (set-mark-command t)
  (end-of-line)
  (kill-ring-save region-beginning region-end)
  )

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

;; (use-package zenburn-theme
;;   :ensure t
;;   :defer t
;;   :init
;;   (load-theme 'zenburn t))

(setq inhibit-startup-screen t)

;; (setq initial-scratch-message nil)

(setq inhibit-startup-message t)

;; (use-package page-break-lines
;;   :ensure t)
;; (use-package all-the-icons
;;   :ensure t)
(use-package dashboard
  :ensure t
  :config
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents . 15) (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 3)
  (dashboard-setup-startup-hook)
  )

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))    
  (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
	      ("C-o" . other-window))
  :init
  (setq vterm-buffer-name-string "vterm %s"))

;; (use-package multi-vterm
;;   :ensure t)

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


(defun mvterm-left ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm))

(global-set-key (kbd "C-c b") 'mvterm-left)
(global-set-key (kbd "C-c j") 'multi-vterm-next)
(global-set-key (kbd "C-c k") 'multi-vterm-prev)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(global-set-key (kbd "C-o") 'other-window)

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))


;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq kill-buffer-query-functions nil)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package eglot
  :hook (python-mode . eglot-ensure))

;; ;;-------------------------------------------------

;; (use-package company
;;   :defer t
;;   :hook (python-mode . company-mode))

;; ;;-------------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
