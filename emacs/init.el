(setq inhibit-startup-message t) ; remove startup message

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode 10) ; set padding of window

(menu-bar-mode -1) ; disable menu bar

(setq visible-bell t) ; set up visible bell

;; set font + style + size
(set-face-attribute 'default nil :font "JetBrains Mono Medium" :height 140)

(load-theme 'wombat) ; setting tango dark theme

;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)

;; initialize use-package on non linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; download packages on-the-fly
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-mode t)
 '(package-selected-packages '(doom-modeline counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(use-package counsel)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
