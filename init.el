;;; init.el -- Jason Legler's Emacs Config
;;;
;;; Commentary:
;;; No Spacemacs, no Doom, no Evil, just bespoke artisinal Emacs.
;;;
;;; Please see README.md for details
;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Init Settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Disable package.el on startup, we'll be using straight.el
(setq package-enable-at-startup nil)


;; Memory performance enhancements
(setq gc-cons-threshold 100000000              ;; Set GC high to limit collections on startup
      read-process-output-max(* 1024 1024 4))  ;; Read 4MB chunks from language server to speed it up


;; Set window startup size to smallest size that doesn't fill me with rage
(setq-default initial-frame-alist '((width . 120)
                                    (height. 24))
              default-frame-alist initial-frame-alist
              frame-inhibit-implied-resize t)


;; Native compile Emacs Lisp for speed
(when(featurep 'native-compile)
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors nil))


;; Load newest config files
(setq load-prefer-newer noninteractive)


;; Bootstrap Straight to manage loading and management of packages
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))        ;; Install straight.el
(straight-use-package 'use-package)            ;; Install use-package

(use-package straight
  :custom (straight-use-package-by-default t)) ;; Use straight.el by default


;; Markdown (declared early to more easily set *scratch* buffer mode)
(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . smartparens-global-strict-mode)
         (markdown-mode . rainbow-delimiters-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-"))
  (setq initial-major-mode 'markdown-mode       ;; Set *scratch* to Markdown mode instead of lisp mode
        initial-scratch-message "# Notes\n\n")) ;; Set buffer content for scratch markdown file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global Look and Feel ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Use preferred theme
(use-package zenburn-theme
  :config (load-theme 'zenburn t))


;; Configure non-terminal emacs window
(when (window-system)
        (set-face-attribute 'default nil
                      :height 144
                      :family "PragmataPro Liga") ;; Set preferred font
  (tool-bar-mode -1)                              ;; Disable toolbar
  (scroll-bar-mode -1)                            ;; Disable scrollbar
  (fringe-mode -1)                                ;; Disable fringe around the frame
                (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))    ;; Use a transparent menu bar
                        (add-to-list 'default-frame-alist
               '(ns-appearance . dark))           ;; Assume a dark colorscheme
  (setq ns-use-proxy-icon nil                     ;; Remove icon from titlebar
        frame-title-format nil                    ;; Remove file name from titlebar
        frame-resize-pixelwise t                  ;; Make frame resize to the pixel
        window-resize-pixelwise t))               ;; Make window resize to the pixel
(menu-bar-mode 0) ;; Disable the menu bar


;; Difficult-to-categorize Keymapping overrides
(global-set-key (kbd "M-g") 'goto-line)           ;; use preferred binding for goto-line
(put 'kill-current-buffer 'disabled t)            ;; disable cmd-k since it is catastrophic and I keep doing it.
(add-hook 'eshell-mode-hook
          #'(lambda () (local-set-key (kbd "s-k") ;; Make cmd-k clear the shell to account for decades of muscle memory
                                     (lambda ()
                                       (interactive)
                                       (let ((inhibit-read-only t))
                                         (erase-buffer)
                                         (eshell-send-input))))))
(add-hook 'sql-interactive-mode-hook
          #'(lambda () (local-set-key (kbd "s-k") ;; Make cmd-k clear the shell to account for decades of muscle memory
                                     (lambda ()
                                       (interactive)
                                       (erase-buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal Global Settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Buffer visual preferences
(show-paren-mode 1)                          ;; Highlight matching parens
(global-hl-line-mode 1)                      ;; Highlight current line
(setq line-number-mode t                     ;; Add line number to status bar
      column-number-mode t)                  ;; Add column number to status bar
(size-indication-mode t)                     ;; Add size of file to status bar
(global-display-line-numbers-mode)           ;; Enable global line numbers
(add-hook 'text-mode-hook
          (lambda () (setq fill-column 80))) ;; Set up text mode to have a default width of 80


;; Behavior preferences
(setq-default fill-column 120)               ;; Set fill column to default to 120 characters
(setq-default indent-tabs-mode nil)          ;; Smart tab behavior-indent or complete
(setq confirm-kill-emacs 'yes-or-no-p        ;; prompt before killing emacs
      inhibit-startup-message t              ;; Go straight to scratch buffer on startup
      help-window-select t                   ;; Always select the help buffer on open
      make-backup-files nil                  ;; Disable backup files
      create-lockfiles nil                   ;; Disable lock files
      ring-bell-function 'ignore             ;; Disable the bell
      select-enable-clipboard t              ;; Integrate kill-ring with copy / paste
      select-enable-primary t                ;; Integrate kill-ring with copy / paste
      save-interprogram-paste-before-kill t) ;; Integrate kill-ring with copy / paste
(fset 'yes-or-no-p 'y-or-n-p)                ;; Change all yes / no questions to y / n
(delete-selection-mode t)                    ;; Delete the selection with a keypress
(global-auto-revert-mode t)                  ;; Revert externally modified buffers automatically


;; Default File Mangling
(setq require-final-newline t                            ;; Add newline at end of all files
      tab-always-indent 'complete)                       ;; Don't use hard tabs by default
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Remove trailing whitespace


;; Move all customization information into its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :no-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global External Package Management ;;;;;;
;;;; (other than straight, zenburn, and ;;;;;;
;; markdown, which were referenced earlier) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Modeline theme
(use-package telephone-line
  :config
  (setq telephone-line-lhs
        '((accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-projectile-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-flymake-segment
                     telephone-line-major-mode-segment
                     telephone-line-misc-info-segment))
          (accent   . (telephone-line-airline-position-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right
        telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode t))


;; Make Emacs commands easier to find as you type
(use-package which-key
  :config
  (which-key-mode))


;; In buffer auto-complete
(use-package company
  :commands company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode) ;; Enable in all buffers
  (setq company-show-numbers t                     ;; Show numbers alongside the completeion dialogue.
        company-dabbrev-downcase nil))             ;; Stop downcasing auth-completion results


;; Annotate the "margins" of the mini-buffer
(use-package marginalia
  :init
  (marginalia-mode))


;; Generic narrowing-completion mechanism for lists in Emacs (instead of Ivy)
(use-package vertico
  :config
  (vertico-mode)
  (global-set-key (kbd "C-s") 'consult-line)              ;; in-buffer search
  (global-set-key (kbd "C-S") 'consult-ripgrep)           ;; ripgrep search
  (global-set-key (kbd "M-X" ) 'execute-extended-command) ;; emacs commands
  (global-set-key (kbd "C-x C-f") `find-file))            ;; find file by fuzzy name


;; Completing read of Emacs commands (instead of counsel)
(use-package consult)


;; Efficient syntax highlighting framework
(use-package tree-sitter
  :config
  (require 'tree-sitter)
  (global-tree-sitter-mode)
  (define-derived-mode typescriptreact-mode typescript-mode "Typescript TSX")        ;; add custom react mode
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))              ;; add custom react mode
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)) ;; add custom react mode
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode)))


;; Language parsers for syntax highlighting
(use-package tree-sitter-langs
  :hook tree-sitter)


;; The best git client ever
(use-package magit)


;; Have TODOs show up in magit
(use-package magit-todos
  :config
  :hook magit-mode)


;; Git gutter, so I can see the damage done
(use-package git-gutter
  :config
  (global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)   ;; Revert hunk where cursor is
  (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)     ;; Jump to next hunks
  (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk) ;; Jump to previous hunk
  (global-git-gutter-mode +1))


;; The best way to manage multiple projects at once in one editor
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/workspace/")
        projectile-globally-ignored-directories '("-/target")
        projectile-completion-system 'ivy))


;; Linting
(use-package flycheck
  :commands global-flycheck-mode)


;; Pull environment variables from shell if using macOS
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH")))) ;; Pull path from shell so commands work correctly


;; Set environment variables with direnv
(use-package direnv
 :config
 (direnv-mode))


;; Make it easier to access what you've copied in the past
(use-package browse-kill-ring)


;; Quick window switching in Emacs
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)     ;; Switch between two windows quickly
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; Set default keybindings for kill-ring
  (browse-kill-ring-default-keybindings))      ;; Switch between lots of windows quickly


;; Parenthesis management
(use-package smartparens
  :config
  (require 'smartparens-config)
  (global-set-key (kbd "C-c b") 'sp-forward-barf-sexp)
  (global-set-key (kbd "C-c f") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "C-c d") 'sp-unwrap-sexp))


;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (require 'rainbow-delimiters))


;; Expand region selection
(use-package expand-region
  :config
  (global-set-key (kbd "C-c n") 'er/expand-region))


;; Make it easy to move lines around
(use-package drag-stuff
  :config
  (drag-stuff-define-keys)  ;; This uses <M-up>, <M-down>, <M-right>, and <M-left>
  (drag-stuff-global-mode))


;; Multiple cursors
(use-package multiple-cursors
  :bind (("M-s-<mouse-1>" . mc/add-cursor-on-click)
         ("M-s-<down>" . mc/mmlte--down)
         ("M-s-<up>" . mc/mmlte--up)
         ("M-s-<left>" . mc/mark-previous-like-this)
         ("M-s-<right>" . mc/mark-next-like-this)
         ("M-s-a" . mc/mark-all-like-this)))


;; persistent scratch
(use-package persistent-scratch
  :config
  (persistent-scratch-autosave-mode 1))


;; LSP integration
(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil             ;; use flycheck
        lsp-enable-which-key-integration t ;; setup whichkey integration for lsp
        lsp-before-save-edits t))          ;; apply lsp suggested edits prior to saving


;; LSP UI integration
(use-package lsp-ui
  :hook lsp-mode
  :config
  (setq lsp-ui-doc-enable nil        ;; disable doc popups, just use modeline
        lsp-ui-sideline-enable nil)) ;; disable sideline, just use modeline


;;; format on save for all the languages
(use-package apheleia
  :config
  (apheleia-global-mode +1))


;; Language snippets
(use-package yasnippet)


;; Postgres Convenience
(defun pg-scratch ()
  (interactive)
  (switch-to-buffer "*sql-scratch*")
  (sql-mode)
  (sql-set-product "postgres")
  (sql-set-sqli-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language Modes ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; Yaml
(use-package yaml-mode
  :commands yaml-mode
  :hook ((yaml-mode . lsp-mode)
         (yaml-mode . (lambda ()
                        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))


;; Typescript/Javascript
;;; Set up typescript mode
(use-package typescript-mode
  :commands typescript-mode
  :hook ((typescript-mode . smartparens-strict-mode)
         (typescript-mode . rainbow-delimiters-mode)
         (typescript-mode . lsp-mode)
         (typescript-mode . jest-test-mode)
         (typescript-mode . tree-sitter-mode))
  :config
  (setq typescript-indent-level 2))

;;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :hook ((typescript-mode . (lambda () (tsi-typescript-mode 1)))
         (json-mode . (lambda () (tsi-json-mode 1)))
         (css-mode . (lambda () (tsi-css-mode 1)))
         (scss-mode . (lambda () (tsi-scss-mode 1)))))

;;; Set up jest testing shortcuts in typescript mode
(use-package jest-test-mode)


;; Web
;; Add tailwind
(straight-use-package
 '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

;; Configure web-mode primarily for working with elixir's phoenix templates
(use-package web-mode
  :mode (("\\.html.heex\\'" . web-mode))
  :hook ((web-mode . lsp-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'lsp-language-id-configuration '(".*\\.html.heex$" . "html")) ;; add heex to lsp languagess
  (setq web-mode-engines-alist'(("html"    . "\\.html.heex\\'"))))           ;; add elixir as engine for heex


;; Go
(use-package go-mode
  :commands go-mode
  :hook ((go-mode . lsp-mode)
         (go-mode . smartparens-strict-mode)
         (go-mode . rainbow-delimiters-mode)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook 'lsp-organize-imports t t)))) ;; fix imports on save
  :config
  (setq-default tab-width 4))


;; Python
(use-package python-mode
  :commands python-mode
  :hook ((python-mode . lsp-mode)                    ;; enable lsp
         (python-mode . smartparens-strict-mode)     ;; enable smartparens
         (python-mode . rainbow-delimiters-mode)     ;; enable rainbow delimiters
         (python-mode . pyvenv-tracking-mode)))      ;; show venv in modeline

;;; Virtual env management
(use-package pyvenv
  :commands python-mode)

;;; Automatic virtualenv management
(use-package auto-virtualenv
  :commands python-mode
  :hook ((python-mode . auto-virtualenv-set-virtualenv)
         (projectile-after-switch-project . auto-virtualenv-set-virtualenv)))


;; Rust
(use-package rustic
  :commands rust-mode
  :hook ((rust-mode . lsp-mode)
         (rust-mode . smartparens-strict-mode)
         (rust-mode . rainbow-delimiters-mode)))

(use-package toml-mode
  :commands toml-mode)


;;  Elixir
(use-package elixir-mode
  :commands elixir-mode
  :hook ((elixir-mode . lsp-mode)
         (elixir-mode . smartparens-strict-mode)
         (elixir-mode . rainbow-delimiters-mode)))

(use-package flycheck-credo
  :commands elixir-mode)


;; Zig
(use-package zig-mode
  :commands zig-mode
  :mode (("\\.zig\\'" . zig-mode))
  :hook ((zig-mode . lsp-mode)))


;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)


;; Sly (Common Lisp)
(use-package sly
  :commands sly-mode
  :mode (("\\.cl$" . sly-mode))
  :hook ((sly-mode . rainbow-delimiters-mode)
         (sly-mode . smartparens-strict-mode)))


;; Clojure
;;; Clojure linter
(use-package flycheck-clj-kondo
  :commands clojure-mode)

;;; Clojure REPL
(use-package cider
  :commands clojure-mode
  :hook ((cider-mode . rainbow-delimiters-mode)
         (cider-mode . smartparens-strict-mode))
  :config
  (setq cider-show-error-buffer                            ;; When there's a cider error, show its buffer
        cider-repl-history-file "~/.emacs.d/cider-history" ;; Where to store the cider history
        cider-repl-display-in-current-window t             ;; When switching to the REPL, show it in the current window
        cider-repl-display-help-banner nil                 ;; Disable the Cider help message
        cider-prompt-for-symbol nil))                      ;; Just go to the symbol under the point; don't ask

;;; Clojure mode
(use-package clojure-mode
  :commands clojure-mode
  :mode (("\\.edn$" . clojure-mode)   ;; associate .edn files
         ("\\.boot$" . clojure-mode)) ;; associate .boot files
  :config (require 'flycheck-clj-kondo)
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . subword-mode)))


(provide 'init)
;;; init.el ends here
