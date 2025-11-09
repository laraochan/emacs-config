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


;; setting font - family & size
(set-face-attribute 'default nil
		    :family "JetBrainsMono"
		    :height 130) ;; 130 = 13pt

(when (equal window-system 'darwin)
  (setq mac-option-modifier 'meta))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Sora Terao")
	    (user-mail-address . "me@larao.dev")
	    (user-login-name . "larao")
	    (create-lockfiles . nil)
	    (tab-width . 4)
	    (debug-on-error . t)
	    (init-file-debug . t)
	    (frame-resize-pixelwise . t)
	    (enable-recursive-minibuffers . t)
	    (history-length . 1000)
	    (history-delete-duplicates . t)
	    (scroll-preserve-screen-position . t)
	    (scroll-conservatively . 100)
	    (mouse-wheel-scroll-amout . '(1 ((control) . 5)))
	    (ring-bell-function . 'ignore)
	    (text-quoting-style . 'straight)
	    (truncate-lines . t)
	    (use-dialog-box . nil)
	    (use-file-dialog . nil)
	    (menu-bar-mode . t)
	    (tool-bar-mode . nil)
	    (scroll-bar-mode . nil)
	    (indnet-tabs-mode . nil)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :global-minor-mode show-paren-mode)

(leaf savehist
  :doc "Save minibuffer history"
  :custom `((savehist-file . ,(locate-user-emacs-file "savehist")))
  :global-minor-mode t)

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
	  ("M-n" . flymake-goto-next-error)
	  ("M-p" . flymake-goto-prev-error))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :global-minor-mode t)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf doom-themes
  :doc "Doom Emacs' Theme Pack"
  :ensure t
  :config
  (load-theme 'doom-solarized-dark t))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode
  :custom ((corfu-auto . t)
	   (corfu-auto-delay . 0)
	   (corfu-auto-prefix . 1)
	   (corfu-popupinfo-delay . 0.1))
  :bind ((corfu-map
	  ("C-s" . corfu-insert-separator))))
(
leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)

(leaf consult
  :doc "Consulting completing-read"
  :ensure t
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :custom ((xref-show-xrefs-function . #'consult-xref)
	   (xref-show-definitions-function . #'consult-xref)
	   (consult-line-start-from-top . t))
  :bind (;; C-c bindings (mode-specific-map)
	 ([remap switch-to-buffer] . consult-buffer)
	 ([remap project-switch-to-buffer] . consult-buffer)

	 ;; M-g bindings (goto-map)
	 ([remap goto-line] . consult-goto-line)
	 ([remap imenu] . consult-imenu)

	 ;; C-M-s bindings
	 ("C-s" . consult-line)
	 ("C-M-s" . nil)
	 ("C-M-s s" . isearch-forward)
	 ("C-M-s C-s" . isearch-forward-regexp)
	 ("C-M-s r" . consult-ripgrep)))

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs"
  :ensure t
  :custom ((affe-highlight-function . 'orderless-highlight-matches)
           (affe-regexp-function . 'orderless-pattern-compiler))
  :bind (("C-M-s r" . affe-grep)
         ("C-M-s f" . affe-find)))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles partial-completion)))))

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(leaf vterm
  :ensure t
  :preface
  (defun c/projectile-run-vterm nil
	(interactive)
	(let* ((project (projectile-completing-read
					 "Select project: "
					 (projectile-relevant-known-projects)))
		   (default-directory project)
		   (vterm-buffer-name (format "*vterm: %s*" (projectile-project-name project))))
	  (if (get-buffer vterm-buffer-name)
		  (switch-to-buffer vterm-buffer-name)
		(vterm vterm-buffer-name))))
  :bind (
		 ([remap projectile-run-vterm] . c/projectile-run-vterm)))

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

(leaf markdown-mode
  :ensure t)

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :hook ((tsx-ts-mode-hook . eglot-ensure))
  :custom ((eldoc-echo-area-use-multiline-p . nil)
           (eglot-connect-timeout . 600)))


(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf diff-hl
  :ensure t
  :global-minor-mode t)

(leaf magit
  :ensure t)

(leaf nyan-mode
  :ensure t
  :global-minor-mode t)

(leaf projectile
  :ensure t
  :global-minor-mode t
  :bind ((projectile-mode-map
		  ("C-c p" . projectile-command-map))))

(leaf vim-jp-radio
  :vc ( :url "https://github.com/vim-jp-radio/vim-jp-radio.el"))

(leaf neotree
  :ensure t
  :custom ((neo-smart-open . t)
		   (neo-create-file-auto-open . t))
  :bind (("C-c e" . neotree-toggle)
		 (neotree-mode-map
		  ("." . neotree-hidden-file-toggle))))

(leaf org
  :init
  (setq org-directory "~/Documents/org"
		org-daily-tasks-file (format "%s/tasks.org" org-directory)
		org-capture-templates '(("d" "Weekdays TODO" entry (file org-daily-tasks-file) "%[~/.emacs.d/assets/org-templates/weekdays-todo.org]" :prepend t)))
  :hook ((org-mode-hook . org-indent-mode)))

(leaf org-roam
  :ensure t
  :custom ((org-roam-directory . "~/Documents/org/roam/")
		   (org-roam-db-autosync-mode . t)))

(leaf org-modern
  :ensure t
  :custom ((org-modern-progress . '("○" "◔" "◑" "◕" "✅"))
			(org-modern-star . '("◉" "○" "●" "◆" "◇")))
  :hook
  ((org-mode-hook . org-modern-mode)
   (org-agenda-finalize-hook . org-modern-agenda)))

;; (leaf eldoc-box
;;   :ensure t
;;   :global-minor-mode eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
