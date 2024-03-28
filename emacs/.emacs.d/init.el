;; -*- lexical-binding: t; -*-

;;; load-path
(defun add-to-load-path (&rest paths)
  "Recursively add `PATHS' to `load-path'."
  (dolist (path paths paths)
    (let ((default-directory
           (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path)))))
(add-to-load-path "elisp")

;;; package.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("melpa-stable" . "https://stable.melpa.org/packages/"))))

;;; use-package.el
(setq use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-compute-statistics t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package blackout
  :ensure t)

(use-package hydra
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package use-package-chords
  :ensure t)

;;; cus-start.el, startup.el, etc.
(use-package cus-start :no-require t
  :custom
  ;; garbage-collection-messages t ; ガベージコレクタの発動時にメッセージを出す
  (user-full-name "ril")
  (user-mail-address "fenril.nh@gmail.com")
  (inhibit-startup-message t)           ; startupメッセージ消去
  (initial-scratch-message "")          ; scratchの初期メッセージ消去
  (load-prefer-newer t)
  (create-lockfiles nil)
  (debug-on-error t)
  (init-file-debug t)
  (frame-resize-pixelwise t)
  (enable-recursive-minibuffers t)
  (scroll-preserve-screen-position t)
  (scroll-conservatively 100)
  (ring-bell-function 'ignore)
  (text-quoting-style 'straight)
  (next-screen-context-lines 1)         ; 画面スクロール時の重複行数
  (scroll-conservatively 35)   ; スクロール行数 (一行ごとのスクロール)
  (scroll-margin 0)
  (scroll-step  1)
  (scroll-preserve-screen-position t) ; スクロール時のカーソル位置の維持
  (ndicate-empty-lines t)
  (display-line-numbers t)
  (enable-recursive-minibuffers t)       ; minibufferの再帰呼び出し許可
  (echo-keystrokes 0.1)
  (message-log-max 1000)          ; ログの記録行数を増やす
  (history-length t)              ; minibufferの履歴のリストを無限大に
  (history-delete-duplicates t)
  (kill-ring-max 2048)
  (undo-no-redo t)                    ; 過去のundoがredoされないように
  (use-short-answers t)               ; y-or-nにする
  (auto-save-list-file-name nil)
  (auto-save-list-file-prefix nil) ; auto-save-list-file-nameを初期化しない
  (auto-save-timeout 10)     ; オートセーブファイル作成までの秒間隔
  (auto-save-interval 500)   ; オートセーブファイル作成までのタイプ間隔
  (truncate-partial-width-windows nil) ; バッファ画面外文字の切り詰め表示
  (tab-width 4)
  (indent-tabs-mode nil)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil))

(use-package frame-title-and-mode-line :no-require t
  :custom
  (frame-title-format `(,(format "%x @" (emacs-pid))
                        (buffer-file-name "%f" "%b")
                        ,(format " - GNU Emacs %s [%%s]" emacs-version)))
  ( mode-line-format '("%e"
                       mode-line-front-space
                       mode-line-mule-info
                       mode-line-client
                       mode-line-modified
                       mode-line-remote
                       mode-line-frame-identification
                       mode-line-buffer-identification
                       " "
                       mode-line-position
                       (vc-mode vc-mode)
                       " "
                       mode-line-modes
                       mode-line-misc-info
                       mode-line-end-spaces))
  ;; `mode-line-position'の行番号等の表示
  (size-indication-mode t)              ; ファイルサイズを表示
  (line-number-mode t)
  (column-number-mode t))

(use-package initial-key :no-require t
  :init
  (global-unset-key (kbd "C-x C-c"))
  (defalias 'exit 'save-buffers-kill-terminal)
  (keyboard-translate ?\C-h ?\C-?)
  (global-set-key (kbd "M-n") 'scroll-down-line)
  (global-set-key (kbd "M-p") 'scroll-up-line))

(use-package trailing-whitespace :no-require t
  :custom
  (show-trailing-whitespace t)
  :init
  (defun my/disable-trailing-mode-hook ()
    "Disable show tail whitespace."
    (setq show-trailing-whitespace nil))
  (defvar my/disable-trailing-modes
    '(comint-mode
      eshell-mode
      eww-mode
      term-mode
      vterm-mode
      Buffer-menu-mode))
  (mapc (lambda (mode)
          (add-hook (intern (concat (symbol-name mode) "-hook"))
                    'my/disable-trailing-mode-hook))
        my/disable-trailing-modes))

;;; Misc
(use-package autorevert
  :demand
  :custom
  (auto-revert-interval 1)
  :config
  (global-auto-revert-mode 1))

(use-package ffap
  :config
  (ffap-bindings))

(use-package find-func
  :config
  (find-function-setup-keys))

(use-package myutils
  :demand
  :init
  (global-unset-key (kbd "C-z"))
  :bind
  ("C-z" . other-window-or-split-or-close)
  ("C-c f" . check-font-here))

(use-package dtw-mode
  :ensure t
  :vc
  (:fetcher github :repo fenril058/dtw-mode)
  :init
  (global-dtw-mode 1)
  :custom
  (dtw-use-disable-list t)
  (dtw-disable-modes
   '(fundamental-mode yatex-mode latex-mode))
  :config
  (defalias 'dtw 'delete-trailing-whitespace))

(use-package duplicate-thing
  :ensure t
  :bind
  ("C-c d" . duplicate-thing))

(use-package sequential-command
  :vc
  (:fetcher github
            :repo fenril058/sequential-command
            :rev "master")
  :config
  (use-package sequential-command-config
    :custom
    (seq-cmd-home-prefer-back-to-indentation t)
    (seq-cmd-end-prefer-end-of-code t)
    :config
    (sequential-command-setup-keys)))

(use-package anzu
  :ensure t
  :blackout
  :bind
  ("C-c r" . anzu-query-replace)        ; M-%
  ("C-c R" . anzu-query-replace-regexp) ; C-M-%
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000))

(use-package dmacro
  :ensure t
  :blackout
  :custom
  (dmacro-key (kbd "<f9>"))
  :config
  (global-dmacro-mode 1))

;;; 日本語
(use-package migemo
  :ensure t
  :demand
  :custom
  (migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-use-pattern-alist t)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1024)
  :bind
  (:map isearch-mode-map
        ("C-t" . migemo-isearch-toggle-migemo))
  :config
  (migemo-init))

(use-package mozc
  :ensure t
  :after key-chord
  :custom
  (default-input-method "japanese-mozc")
  :preface
  (defadvice toggle-input-method (around toggle-input-method-around activate)
    "Input method function in key-chord.el not to be nil."
    (let ((input-method-function-save input-method-function))
      ad-do-it
      (setq input-method-function input-method-function-save)))
  :config
  (defun my:mozc-config-dialog ()
    "Run the mozc-tool in the background."
    (interactive)
    (async-shell-command
     "/usr/lib/mozc/mozc_tool --mode=config_dialog")
    (delete-other-windows))

  (defun my:mozc-dictionary-tool ()
    "Run the mozc-tool in the background."
    (interactive)
    (async-shell-command
     "/usr/lib/mozc/mozc_tool --mode=dictionary_tool")
    (delete-other-windows))

  (defun my:mozc-word-regist ()
    "Run the mozc-tool in the background."
    (interactive)
    (async-shell-command
     "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
    (delete-other-windows))
  (defun my:mozc-hand-writing ()
    "Run the mozc-tool in the background."
    (interactive)
    (async-shell-command
     "/usr/lib/mozc/mozc_tool --mode=hand_writing")
    (delete-other-windows)))

(use-package mule-cmds
  :init
  (defun set-cursor-color-slateblue2 ()
    (set-cursor-color "SlateBlue2"))
  (defun set-cursor-color-indianred ()
    (set-cursor-color "indian red"))
  :hook
  (input-method-activate . set-cursor-color-slateblue2)
  (input-method-deactivate . set-cursor-color-indianred))

;;; dired
(use-package dired
  :custom
  (dired-listing-switches "-lGhF")
  :config
  (defun toggle-dired-list-all ()
    "Toggle whether list dot files."
    (interactive)
    (dired-sort-other
     (setq dired-listing-switches
           (if (string-match "[Aa]" dired-listing-switches)
               "-lGhF"
             "-lGhFA"))))
  :bind
  (:map dired-mode-map
        (")" . toggle-dired-list-all)))

(use-package dired-x)

(use-package wdired
  :bind
  (:map dired-mode-map
        ("r" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t))

;;; History
(use-package savehist
  :config
  (savehist-mode 1))

(use-package recentf
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 10000)
  (recentf-exclude '(".recentf" "*.elc" "/TAGS$" "/var/tmp/"))
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package auto-save-recentf
  :config
  (enable-auto-save-recentf))

(use-package auto-save-buffers-enhanced
  :ensure t
  :blackout
  :config
  (setq auto-save-buffers-enhanced-interval 180.0)
  (auto-save-buffers-enhanced t))

(use-package files
  :custom
  (make-backup-files nil)         ; 変更ファイルのバックアップ
  (version-control nil)           ; 変更ファイルの番号つきバックアップ
  (kept-old-versions 1)           ; 古いものをいくつ残すか
  (kept-new-versions 2)           ; 新しいものをいくつ残すか
  (trim-versions-without-asking nil) ; バックアップファイル上書き時の警告をオフ
  (delete-old-versions t))           ; 古いバックアップファイルの削除

(use-package savekill
  :ensure t)

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package undo-tree
  :ensure t
  :blackout
  :bind
  ("C-M-/" . undo-tree-redo)
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package winner
  :requires hydra
  :bind
  ("C-c u" . hydra-winner/body)
  :config
  (winner-mode 1)
  (defhydra hydra-winner ()
    "Winner"
    ("u" winner-undo "undo")
    ("U" winner-redo "redo" :exit t)))

(use-package goto-chg
  :ensure t
  :bind
  ("M-g l" . hydra-goto-chg/body)
  :config
  (defhydra hydra-goto-chg ()
    "Goto last change"
    ("j" goto-last-change "goto last change")
    ("k" goto-last-change-reverse "goto last change reverse")))

;;; Paren
(use-package elec-pair
  :config
  (add-to-list 'electric-pair-pairs '(?\（ . ?\）))
  (add-to-list 'electric-pair-pairs '(?\「 . ?\」))
  :config
  (electric-pair-mode 1))

;;; Visual
(use-package generic-x)

(use-package whitespace
  :blackout global-whitespace-mode
  :custom
  (whitespace-style
   '(face tabs trailing space-before-tab space-after-tab tab-mark))
  :custom-face
  (whitespace-tab ((((background dark))
                    :underline t
                    :background unspecified
                    :foreground "Darkred"
                    )))
  :init
  (global-whitespace-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (lisp-interaction-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (ruby-mode . rainbow-delimiters-mode)
  (python-mode . rainbow-delimiters-mode)
  (c-mode-common . rainbow-delimiters-mode)
  (c-ts-mode . rainbow-delimiters-mode)
  (c++-ts-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((((class color) (background dark))
                                     (:foreground "#EEEEEE"))))
  (rainbow-delimiters-depth-2-face ((((class color) (background dark))
                                     (:foreground "#f0df1f"))))
  (rainbow-delimiters-depth-3-face ((((class color) (background dark))
                                     (:foreground "#94bff3"))))
  (rainbow-delimiters-depth-4-face ((((class color) (background dark))
                                     (:foreground "#dca3a3"))))
  (rainbow-delimiters-depth-5-face ((((class color) (background dark))
                                     (:foreground "#8fb28f"))))
  (rainbow-delimiters-depth-6-face ((((class color) (background dark))
                                     (:foreground "#7cb8bb"))))
  (rainbow-delimiters-depth-7-face ((((class color) (background dark))
                                     (:foreground "#dfaf8f"))))
  (rainbow-delimiters-depth-8-face ((((class color) (background dark))
                                     (:foreground "#dc8cc3"))))
  (rainbow-delimiters-depth-9-face ((((class color) (background dark))
                                     (:foreground "#d0bf8f")))))

(use-package rainbow-mode
  :ensure t
  :blackout
  :hook
  (prog-mode . rainbow-turn-on))

(use-package which-func
  :custom-face
  (which-func ((t
                (:backgroung unspecified :foreground unspecified))))
  :config
  (setq mode-line-misc-info
        (assq-delete-all 'which-function-mode mode-line-misc-info))
  (setq-default header-line-format
                '((which-function-mode ("" which-func-format " "))))
  (which-function-mode 1))

(use-package highlight-symbol
  :ensure t
  :custom
  (highlight-symbol-idle-delay 1.0)
  :bind
  ("<f5>" . hydra-highlight-symbol/body)
  :config
  (defhydra hydra-highlight-symbol ()
    ("<f5>" highlight-symbol)
    ("n" highlight-symbol-next "next")
    ("p" highlight-symbol-prev "previous")
    ("r" highlight-symbol-query-replace "query replace" :exit t)
    ("t" highlight-symbol-mode "highlight-symbol-mode")
    ("S-<f5>" highlight-symbol-nav-mode "nav-mode")))

(use-package hl-column
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :blackout
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  :hook
  (python-mode . highlight-indent-guides-mode))

;;; Font
(use-package faces
  :custom-face
  (mode-line ((t (:family "Rounded Mgen+ 1c" :weight regular))))
  (mode-line-inactive ((t (:family "Rounded Mgen+ 1c" :weight regular))))
  (variable-pitch ((t (:family "Mgen+ 1cp"))))
  (fixed-pit ((t (:familiy "Moralerspace Around HWNF"))))
  :init
  (setq use-default-font-for-symbols nil)
  (set-frame-font "Moralerspace Argon HWNF")
  (set-fontset-font
   t 'unicode (font-spec :family "Moralerspace Argon HWNF"))
  (set-fontset-font
   t 'japanese-jisx0213.2004-1 (font-spec :family "Mgen+ 1c") nil 'append)
  (create-fontset-from-ascii-font
   "Mgen+1c:weight=regular:slant=normal" nil "variable")
  (set-fontset-font
   "fontset-variable" 'japanese-jisx0213.2004-1 (font-spec :family "Mgen+ 1cp"))
  (set-fontset-font
   "fontset-variable" '(#x1F000 . #x1FAFF) "Noto Color Emoji" nil 'append)
  (set-face-attribute
   'variable-pitch nil :fontset "fontset-variable")
  (use-package noto-color-emoji-codepoint
    :config
    (add-noto-color-emoji-to-fontset nil)
    (add-noto-color-emoji-to-fontset "fontset-variable"))
  (add-to-list 'face-font-rescale-alist '("Noto Color Emoji" . 0.9))
  (add-to-list 'face-font-rescale-alist '("Mgen" . 1.1)))

;;; 補完
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init
  (global-corfu-mode 1))

(use-package corfu-popupinfo
  :after corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-terminal
  :ensure t)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t)

(use-package tempel
  :ensure t
  :bind
  ("M-+" . tempel-complete) ;; Alternative tempel-expand
  ("M-*" . tempel-insert)
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package yasnippet
  :ensure t
  :requires warnings
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1))

(use-package hippie-exp
  :bind
  (("C-;" . hippie-expand))
  :custom
  ((hippie-expand-try-functions-list
    '(try-complete-file-name-partially
      try-complete-file-name
      try-expand-all-abbrevs
      try-expand-dabbrev
      try-expand-dabbrev-all-buffers
      try-expand-dabbrev-from-kill
      try-complete-lisp-symbol-partially
      try-complete-lisp-symbol
      ))))

(use-package autoinsert
  :custom
  (auto-insert-directory "~/.emacs.d/insert/")
  :config
  (add-to-list 'auto-insert-alist '("\\.tex$" . "latex-insert.tex" ))
  (auto-insert-mode 1))

;;; vertico, consult, etc.
(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode 1))

(use-package consult
  :ensure t
  :after myutils
  :demand
  :bind
  ("C-c i" . consult-ripgrep-single-file)
  ("C-x b" . consult-buffer)                ; default switch-to-buffer
  ([remap goto-line] . consult-goto-line)   ; goto-line を置き換え
  (:map overriding-minor-mode-map
        ("C-:" . consult-buffer))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-preview-key  '(:debounce 0.5 any))
  (consult-ripgrep-args
   (concat "rg "
           "-uu " ; -u がないと .gitignore のせいで何もヒットしないことがある.
           "--null "
           "--line-buffered "
           "--color=never "
           "--max-columns=1000 "
           "--path-separator /\ "
           "--smart-case "
           "--no-heading "
           "--with-filename "
           "--line-number "
           "--search-zip "))
  :config
  (use-package consult-xref)
  (defun consult-ripgrep-single-file ()
    "Call `consult-ripgrep' for the current buffer (a single file)."
    (interactive)
    (let ((consult-project-function (lambda (_) nil))
          (consult-ripgrep-args
           (concat "rg "
                   "--null "
                   "--line-buffered "
                   "--color=never "
                   "--line-number "
                   "--smart-case "
                   "--no-heading "
                   "--max-columns=1000 "
                   "--max-columns-preview "
                   "--with-filename "
                   (shell-quote-argument buffer-file-name))))
      (consult-ripgrep))))

(use-package consult-ghq :ensure t)

(use-package consult-yasnippet
  :ensure t
  :bind
  ("C-c y" . consult-yasnippet))

(use-package orderless
  :ensure t
  :after migemo
  :config
  ;; migemo
  (defun orderless-migemo (component)
    "Match COMPONENT as a migemo-string."
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-migemo)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package embark-consult
  :ensure t
  :after consult
  :bind
  ("C-c e" . embark-act))

(use-package affe
  :ensure t
  :after orderless consult
  :custom
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (affe-find-command "fd --color=never --full-path")
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-compile input))
    (cons input (apply-partially #'orderless--highlight input)))
  (eval-and-compile
    (require 'consult)
    (consult-customize affe-grep :preview-key "M-.")))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-enable-key "r")
  (wgrep-auto-save-buffer t))

;;; imenu
(use-package imenu-list
  :ensure t
  :bind
  (:map imenu-list-major-mode-map
        ("j" . next-line)
        ("k" . previous-line))
  :chords
  ("mn" . imenu-list))

;;; flymake
(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-next-error)))

;;; flycheck
(use-package flycheck
  :ensure t
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))
  :config
  (eval-and-compile
    (if (executable-find "npm")
        (flycheck-define-checker textlint
          "A linter for text."
          :command ("npx" "textlint" "--format" "unix" source)
          :error-patterns
          ((warning line-start (file-name) ":" line ":" column ": "
                    (id (one-or-more (not (any " "))))
                    (message (one-or-more not-newline)
                             (zero-or-more "\n" (any " ")
                                           (one-or-more not-newline)))
                    line-end))
          :modes (text-mode org-mode markdown-mode review-starter-mode))
      (add-to-list 'flycheck-checkers 'textlint))))

(use-package flycheck-inline
  :ensure t
  :hook
  (flycheck-mode . flycheck-inline-mode))

(use-package consult-flycheck
  :ensure t)

;;; Tree-sitter
(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode 1))

;;; Hide code/comment block
(use-package hideshow
  :hook
  (emacs-lisp-mode . hs-minor-mode)
  (c-ts-mode-common . hs-minor-mode)
  (c++-ts-mode-common . hs-minor-mode)
  (go-ts-mode . hs-minor-mode)
  (python-ts-mode . hs-minor-mode))

;;; Emacs lisp
(use-package paredit
  :preface
  (defun lisp-interaction-mode--C-j ()
    (if (eq major-mode 'lisp-interaction-mode)
        (define-key paredit-mode-map (kbd "C-j") nil)
      ;; (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
      ))
  :hook
  (paredit-mode . lisp-interaction-mode--C-j)
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (ilem-mode . enable-paredit-mode))

(use-package eldoc
  :ensure t
  :hook
  (emacs-lisp-mode . turn-on-eldoc-mode)
  (lisp-interaction-mode . turn-on-eldoc-mode)
  (ielm-mode . turn-on-eldoc-mode)
  :custom
  (eldoc-idle-delay 0.1))

(use-package macrostep
  :ensure t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-e" . macrostep-expand)))

(use-package auto-async-byte-compile
  :ensure t
  ;; 自作マクロは ~/.emacs.d/initfuncs.el に移動すること
  :custom
  ((auto-async-byte-compile-exclude-files-regexp
    "\\(/junk/\\|/elpa/\\)"))
  :hook
  (emacs-lisp-mode . enable-auto-async-byte-compile-mode))

;;; C language
(use-package c-ts-mode
  :bind
  (:map c-ts-base-mode-map
        ("C-c C-c" . compile))
  :hook
  (c-ts-mode . c-ts-mode-toggle-comment-style)
  (c-ts-mode . eglot-ensure))

(use-package hideif
  :hook
  (c-mode-common . hide-ifdef-mode)
  (c-ts-mode . hide-ifdef-mode)
  (c++-ts-mode . hide-ifdef-mode))

(use-package clang-format
  :ensure t)

(use-package cmake-mode
  :ensure t)

;;; Haskell
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . eglot-ensure))

;;; Rust
(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :custom
  (rustic-lsp-client 'eglot) ; default lsp
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-format-trigger 'on-save)
  (rustic-rustfmt-bin "~/.cargo/bin/rustfmt")
  :config
  (add-to-list 'rustic-rustfmt-config-alist '("edition" . "2018"))
  :hook
  (rustic-mode . tree-sitter-hl-mode))

;;; Python
(use-package python
  :hook
  (python-ts-mode . eglot-ensure))

;;; Go
(use-package go-mode
  :ensure t
  :hook
  (go-ts-mode . eglot-ensure))

;;; bnf
(use-package ebnf-mode
  :ensure t
  :mode ("\\.ebnf$" . ebnf-mode)
  :init
  (setq ebnf-mode-production-re "^\\s-*\\([[:alnum:]_-]+\\)\\s-*=")
  :custom
  (ebnf-mode-comment-starter "(*")
  (ebnf-mode-comment-ender "*)")
  (ebnf-mode-eop-char ?\;)
  :config
  (modify-syntax-entry ?- "_" ebnf-mode-syntax-table)
  (modify-syntax-entry ?\; "." ebnf-mode-syntax-table)
  (modify-syntax-entry ?\n "." ebnf-mode-syntax-table)
  (modify-syntax-entry ?' "\"" ebnf-mode-syntax-table)
  (modify-syntax-entry ?\( "()1" ebnf-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" ebnf-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" ebnf-mode-syntax-table))

;;; Latex
(use-package auctex
  :ensure t
  :custom
  (TeX-command-output-list '(("LaTeXMk" ("pdf")) ("LuaLaTeX" ("pdf"))))
  (TeX-electric-sub-and-superscript t)
  (TeX-source-correlate-start-server t)
  (LaTeX-indent-level 4)
  (LaTeX-item-indent 2)
  (TeX-auto-save t)                     ; Enable parse on save.
  (TeX-parse-self t)                    ; Enable parse on load.
  (TeX-master nil)
  (LaTeX-electric-left-right-brace t)   ; カッコの補完.
  (TeX-electric-math (cons "$" "$"))
  (TeX-view-program-selection '((output-pdf "PDF Tools")
                                (output-dvi "xdvi")))
  :config
  (use-package tex-jp
    :custom
    (TeX-default-mode 'japanese-latex-mode)
    (japanese-LaTeX-default-style "jsarticle")
    (japanese-TeX-engine-default 'uptex)
    :mode
    (("\\.tex\\'" . japanese-latex-mode)))
  (use-package kinsoku
    :custom
    (kinsoku-limit 10))
  (use-package reftex
    :custom
    (reftex-plug-into-AUCTeX  t)
    (reftex-insert-label-flags      ; 数式のラベルも自分でラベルを入力
     '("s" "sfte")))

  (use-package texmathp)
  (use-package tex
    :config
    (add-to-list 'TeX-command-list
                 '("make" "make"
                   TeX-run-command nil t
                   :help "Run Make"))
    (add-to-list 'TeX-command-list
                 '("LuaLaTeX" "lualatex %t"
                   TeX-run-TeX nil (latex-mode)
                   :help "Run LuaLaTeX"))
    (add-to-list 'TeX-command-list
                 '("pBibTeX" "pbibtex -kanji=utf8 %s"
                   TeX-run-BibTeX nil t
                   :help "Run pBibTeX"))
    (add-to-list 'TeX-command-list
                 '("upBibTeX" "upbibtex %s"
                   TeX-run-BibTeX nil t :help "Run upBibTeX"))
    (add-to-list 'TeX-command-list
                 '("Mendex" "mendex -U %s"
                   TeX-run-command nil t
                   :help "Create index file with mendex"))
    (add-to-list 'TeX-command-list
                 '("Latexmk" "latexmk -pdfdvi %t"
                   TeX-run-TeX nil (latex-mode) :help "Run LaTeXmk")))
  (defun my-LaTeX-mode-hooks ()
    (turn-on-reftex)
    (TeX-source-correlate-mode)
    (TeX-PDF-mode)
    (LaTeX-math-mode)
    (outline-minor-mode)
    (setq TeX-command-default "LatexMk"))
  :hook
  (LaTeX-mode . my-LaTeX-mode-hooks))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward))
  :config
  (pdf-tools-install))

;;; Howm
(use-package howm
  :ensure t
  :mode (("\\.howm\\'" . org-mode))
  :custom
  (howm-menu-file "~/howm/0000-00-00-000000.txt")
  (howm-menu-lang 'ja)
  ;; 自動保存対策 (非同期実行すればいい?)
  (howm-menu-refresh-after-save nil)  ; メニューを自動更新しない
  (howm-refresh-after-save nil)       ; 下線を引き直さない
  (howm-list-recent-title t)    ; 「最近のメモ」一覧時にタイトル表示
  (howm-list-all-title t)       ; 全メモ一覧時にタイトル表示
  ;; メニューの予定表の表示範囲
  (howm-menu-schedule-days-before 1)  ; 1 日前から
  (howm-menu-schedule-days 7)         ; 7 日後まで
  :hook
  (howm-mode . howm-mode-set-buffer-name)
  (after-save . howm-mode-set-buffer-name)
  :config
  (defun howm-save-and-kill-buffer ()
    (interactive)
    (when (and
           (buffer-file-name)
           (string-match "\\.howm"
                         (buffer-file-name)))
      (save-buffer)
      (kill-buffer nil)))
  :bind
  (:map overriding-minor-mode-map
        ("C-c , ," . howm-menu)
        ("C-c , c" . howm-create)
        )
  (:map howm-mode-map
        ("C-x C-c" . howm-save-and-kill-buffer)
        ("C-x C-j" . howm-menu)))

(use-package japanese-holidays
  :ensure t
  :after calendar
  :config
  (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
        (append japanese-holidays
                holiday-local-holidays
                holiday-other-holidays))
  (setq calendar-mark-holidays-flag t) ; 祝日をカレンダーに表示
  ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
  ;; 変数はデフォルトで設定済み
  (setq japanese-holiday-weekend '(0 6)     ; 土日を祝日として表示
        japanese-holiday-weekend-marker     ; 土曜日を水色で表示
        '(holiday nil nil nil nil nil japanese-holiday-saturday))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  ;; “きょう”をマークするには以下の設定を追加します。
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

;;; org-mode
(use-package org
  :ensure t
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l". org-store-link)
  :custom
  (org-directory "~/memo/")
  (org-default-notes-file (concat org-directory "agenda.org"))
  :mode
  ("\\.org\\(_archive\\)?$" . org-mode))

(use-package org-agenda
  :bind
  ("C-c g" . open-my-agenda-file)
  (:map org-agenda-keymap
        ("q" . quit-window))
  :custom
  (org-refile-targets  `(("agenda.org" :maxlevel . 1)
                         ("someday.org" :level . 2)
                         ("~/memo/agenda.org_archive" :maxlevel . 1)))
  (org-archive-location "%s_archive::* Archived Tasks")
  (org-todo-keywords
   '((sequence "TASK(t)" "STARTED(s)" "WAITING(w)"
               "|" "DONE(x)" "CANCELED(c)" "DEFERRED(f)")
     (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)" "DEFERRED(f)")))
  (org-agenda-files (list org-default-notes-file
                          "~/memo/birthday.org"))
  :config
  (defun open-my-agenda-file ()
    (interactive)
    (find-file org-default-notes-file)))

(use-package org-capture
  :custom
  (org-capture-templates
   '(("n" "Note" entry
      (file+headline "~/memo/agenda.org" "Inbox")
      "** %?\n %i\n   %T\n   %a\n")
     ("t" "Task" entry
      (file+headline "~/memo/agenda.org" "Inbox")
      "** TASK %?\n %i\n   %T\n   %a\n")
     ("b" "Bug" entry
      (file+headline "~/memo/agenda.org" "Inbox")
      "** TASK %?   :bug:\n %i\n   %T\n   %a\n")
     ("j" "Journal" entry
      (file+olp+datetree "~/memo/journal.org" "Journal")
      "* %?\n %i\n  Added: %U \n  From:  %a\n" )
     ("m" "Memo (Clip)" entry
      (file+headline "~/memo/journal.org" "Clip")
      "** %?   :MEMO:\n %i\n %U\n %a\n")
     ("i" "Idea" entry
      (file+headline "~/memo/journal.org" "New Ideas")
      "** %?\n %i\n   %T\n   %a\n")
     ("R" "To Read" entry
      (file+headline "~/memo/someday.org" "Books to Read")
      "** %?\n %i\n   %T\n   %a\n")
     )))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :bind
  (:map markdown-mode-map
        ("C-c C-c C-c" . compile)
        ("C-c C-c RET" . compile)
        ("C-c RET" . compile)))

(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets
        (list
         "https://raw.githubusercontent.com/yzane/vscode-markdown-pdf/d50e168f9bc6f05e00382da8670dd43b8a65995a/styles/markdown-pdf.css"
         "https://raw.githubusercontent.com/yzane/vscode-markdown-pdf/d50e168f9bc6f05e00382da8670dd43b8a65995a/styles/markdown.css"
         "https://raw.githubusercontent.com/yzane/vscode-markdown-pdf/d50e168f9bc6f05e00382da8670dd43b8a65995a/styles/tomorrow.css"
         )))

;;; Shell
(use-package vterm
  :ensure t)

(use-package shell-pop
  :ensure t
  :bind
  ("C-c s" . shell-pop)
  :init
  (defun dired-start-shell (arg)
    "Execute `shell-pop' or `shell' and then pasete ARG if they are non-nil.
ARG is the markeed files in `dired-mode'. Ofcouse, this function
is only valid in `dired-mode'."
    (interactive "P")
    (let ((files (mapconcat 'shell-quote-argument
                            (dired-get-marked-files (not arg))
                            " ")))
      (if (fboundp 'shell-pop)
          (shell-pop arg)
        (shell t))
      (save-excursion (insert " " files))))
  (define-key dired-mode-map [remap dired-do-shell-command] 'dired-start-shell))

;;; Others
(use-package perspective
  :ensure t
  :demand
  :after consult
  :bind
  (:map persp-mode-map
        ("c" . nil)
        ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-frame-global-perspective-include-scratch-buffer t)
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-state-default-file nil)
  :config
  (persp-mode 1)
  (add-to-list 'consult-buffer-sources 'persp-consult-source)
  (setq consult-buffer-sources (delq 'consult--source-buffer consult-buffer-sources))
  (defun toggle-including-consult-source-buffer ()
    "Toggle whether `consult-buffer-sources' includes `consult--source-buffer'."
    (interactive)
    (let ((src (quote consult--source-buffer))
          (srcs (quote consult-buffer-sources)))
      (if (memq src (eval srcs))
          (progn
            (set srcs (delq src (eval srcs)))
            (message "Exclude %s in %s." src srcs))
        (add-to-list srcs src)
        (message "Include %s in %s." src srcs)))))


(use-package number-perspective
  :demand
  :config
  (num-persp-generate-perspective)
  (num-persp-assign-keys)
  (persp-switch "1"))

(use-package viewer
  :ensure t
  :bind
  (:map view-mode-map
        ("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("J" . View-scroll-line-forward)
        ("K" . View-scroll-line-backward))
  :chords
  ("kj" . view-mode)
  :custom
  (view-read-only t)
  (viewer-modeline-color-unwritable "tomato")
  (viewer-modeline-color-view "orange")
  (view-mode-by-default-regexp "\\.log$")
  :hook
  (view-mode . viewer-change-modeline-color-setup)
  (read-only-mode . viewer-stay-in-setup))

(use-package which-key
  :ensure t
  :blackout
  :custom
  (which-key-side-window-max-height 0.6)
  (which-key-idle-delay 0.6)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package mwim
  :vc
  (:fetcher "https://github.com/alezost/mwim.el"))

(use-package yawc-mode
  :vc
  (:fetcher github :repo fenril058/yawc-mode)
  :custom
  (yawc-mode-jp t)
  :hook
  (org-mode . yawc-mode)
  (markdown-mode . yawc-mode))

(use-package open-junk-file
  :ensure t
  :bind
  ("C-c z" . open-junk-file)
  :custom
  (open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S."))

(use-package oblique-strategies-ed5)

(use-package oblique-strategies
  :ensure t
  :vc
  (:fetcher github :repo wandersoncferreira/oblique-strategies))
