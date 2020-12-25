(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; set up use-package functionality
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" default))
 '(package-selected-packages
   '(counsel-projectile projectile hydra evil-collection evil general ivy-rich which-key rainbow-delimiters counsel ivy doom-modeline use-package atom-one-dark-theme dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; turn on visible bell to surpress sound
(setq visible-bell t)

;; hide toolbar, scrollbar and menu bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq inhibit-startup-message t)

;; Make EXC exit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; resize splits
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; set used font
(set-frame-font "FiraCode Medium 11" nil t)

;; start with maximized window
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; load one dark theme
(load-theme 'atom-one-dark t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; disable numbers for some modes:
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
	       

;; PACKAGES
;; ------------------------------------

;; DOOM-MODELINE
;; use doom-modeline as status bar
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
;; ALL-THE-ICONS
;; install mandatory icons for doom-modeline
(use-package all-the-icons) ;; use all-the-icons-install-fonts after installation

;; IVY
;; set up ivy completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; RAINBOW DELIMITERS
;; color parantheses, brackets, etc.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; IVY-RICH
;; get addiotional information in ivy listings
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; COUNSEL
;; various completion functions if ivy is installed
(use-package counsel
  :bind (("M-x" . counsel-M-x) ;; use better M-x functionality
	 ("C-x b" . counsel-switch-buffer) ;; switching buffers with counsel
	 ("C-x C-f" . counsel-find-file) ;; using find file with fuzzy find
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; hide ^ when searching

;; EVIL-MODE
;; using VIM keybindings
;;define leader key
(use-package general
  :config
  (general-create-definer jk1006/leader-keys ;; setting SPC as leader in evil modes, C-SPC in other modes
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (jk1006/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme") ;; setting leader keybindings
    "p" '(projectile-command-map :which-key "projectile")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; go back to normal mode with standard emacs binding

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; EVIL-COLLECTION
;; several keybinding additions to evil-mode
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; HYDRA
;; create repeatable keybindings
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; zoom in and out using repeatable keys
(jk1006/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; PROJECTILE
;; using projectile to manage different coding projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; setting up ivy as completion system for projectile (instead of ido)
  :init
  (when (file-directory-p "~/personal_workspace") ;; search for projects in this path
    (setq projectile-project-search-path '("~/personal_workspace")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

