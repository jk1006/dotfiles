
(setq inhibit-startup-message t) ;; do not display startup message

(scroll-bar-mode -1) ;; remove scroll bar
(tool-bar-mode -1) ;; remove tool bar
(tooltip-mode -1) ;; disable tooltip mode
(set-fringe-mode 10) ;; 

(menu-bar-mode -1)
(setq visible-bell t)

(set-frame-font "JetBrains Mono-13:Semibold" nil t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

(setq default-directory "/Users/d068796/")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
        
(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel
  :after ivy
  :config (counsel-mode))

(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x B") 'counsel-switch-buffer-other-window)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode magit counsel-projectile projectile hydra helpful ivy-rich which-key rainbow-delimiters doom-themes doom-modeline use-package counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq split-height-threshold nil)
(setq split-width-threshold 0)

(column-number-mode)
(global-display-line-numbers-mode t)
;; display line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(setq mac-command-modifier 'meta
        mac-right-option-modifier 'none
        mac-option-modifier 'none)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/personal_workspace")
    (setq projectile-project-search-path '("~/personal_workspace")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package hydra)

(bind-key* "C-c w"
	   (defhydra hydra-resize-windows (:hint nil)
	     ""
	     ("<" shrink-window-horizontally "-narrower-")
	     (">" enlarge-window-horizontally "-wider-")
	     ("-" shrink-window "|shorter|")
	     ("+" enlarge-window "|longer|")
	     ("=" balance-windows "equal")
	     ("q" nil)))

(bind-key* "C-c b"
	   (defhydra hydra-buffer-commands (:hint nil)
	     ""
	     ("m" buffer-menu "menu")
	     ("M" buffer-menu-other-window "menu in split")))

(bind-key* "C-c g"
	   (defhydra hydra-magit (:hint nil)
	     ("s" magit-status "status")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))


