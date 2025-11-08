;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(tool-bar-mode -1)
(when (equal window-system 'darwin)
  (setq mac-option-modifier 'meta))

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-dark-high-contrast t))

(leaf corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto t
	corfu-quit-no-match 'separator))
(leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf vterm
  :ensure t)

(leaf treesit
  :config
  (setq treesit-font-lock-level 4
	treesit-language-source-alist
	'((json "https://github.com/tree-sitter/tree-sitter-json")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
	  (message "treesit: %s is already installed" lang)
	(message "treesit: %s is not installed" lang)
	(treesit-install-language-grammar lang)))))
(leaf json-ts-mode
  :mode
  ("\\.json\\'" . json-ts-mode))
(leaf tsx-ts-mode
  :mode
  (("\\.ts[x]?\\'" . tsx-ts-mode)
   ("\\.[m]ts\\'" . tsx-ts-mode)
   ("\\.js[x]?\\'" . tsx-ts-mode)
   ("\\.[mc]js\\'" . tsx-ts-mode)))

(leaf eglot
  :ensure t
  :config
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure))

(leaf git-gutter
  :ensure t
  :init
  (global-git-gutter-mode))

(leaf magit
  :ensure t)

(leaf nyan-mode
  :ensure t
  :config
  (nyan-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "Aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "21d2bf8d4d1df4859ff94422b5e41f6f2eeff14dd12f01428fa3cb4cb50ea0fb"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
