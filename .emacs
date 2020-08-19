(require 'package)

(add-to-list 'load-path "~/.emacs.d/local/")

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

; (global-wakatime-mode)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-define-key nil evil-insert-state-map
    (kbd "C-l") 'evil-forward-char
    (kbd "C-h") 'evil-backward-char
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line

    (kbd "C-f") 'evil-forward-char
    (kbd "C-b") 'evil-backward-char
    (kbd "C-a") 'evil-insert-line
    (kbd "C-e") 'evil-append-line
    ;; (kbd "C-n") 'evil-next-line
    ;; (kbd "C-p") 'evil-previous-line
    (kbd "C-g") 'evil-normal-state))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  :bind (:map evil-normal-state-map
              ("H" . drag-stuff-up)
              ("L" . drag-stuff-down)
              :map evil-visual-state-map
              ("H" . drag-stuff-up)
              ("L" . drag-stuff-down)))

(use-package ack
  :config
  (defun my-ack-default-directory (_arg)
    (or (ack-guess-project-root default-directory)
        (read-directory-name "In directory: " nil nil t)))
  (setq ack-default-directory-function #'my-ack-default-directory))

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'elixir-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package base16-theme
  :init
  :config
  (setq base16-theme-256-color-source "colors")
  (load-theme 'base16-tomorrow-night t)
  ;; Without this line the 'black' color will not
  ;; be correct in terminal emacs
  (add-to-list 'default-frame-alist '(background-color . "black")))

(use-package cider
  :commands (cider-mode)
  :init
  (setq cider-prompt-for-symbol nil)
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;            (figwheel-sidecar.repl-api/start-figwheel!)
  ;;            (figwheel-sidecar.repl-api/cljs-repl))")
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :config
  (evil-leader/set-key
    "cd" 'cider-eval-defun-at-point
    "cs" 'cider-eval-sexp-at-point
    ;; "rl" 'cider-load-buffer-and-switch-to-repl-buffer
    "rc" 'cider-switch-to-last-clojure-buffer
    "rr" (lambda ()
           (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively #'cider-switch-to-repl-buffer)))
    "rs" 'cider-eval-last-sexp-to-repl
    "bd" 'cider-eval-defun-at-point
    "bs" 'cider-eval-sexp-at-point
    "dg" 'cider-grimoire
    "dc" 'cider-doc))

(use-package smartparens
  :config
  (require 'smartparens-config)
  ;; (smartparens-global-strict-mode t)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; TODO: need to require this here or remove-if errors
;; just use a different function here
;; (require 'cl)

;; (use-package evil-cleverparens
;;   :init
;;   (setq evil-cleverparens-use-regular-insert t)
;;   ;; (setq evil-cleverparens-use-additional-movement-keys nil)
;;   (setq evil-cp-additional-movement-keys
;;         '(("(" . evil-cp-previous-opening)
;;           (")" . evil-cp-next-closing)
;;           ("[" . evil-cp-next-opening)
;;           ("]" . evil-cp-previous-closing)
;;           ("{" . evil-cp-backward-up-sexp)
;;           ("}" . evil-cp-up-sexp)))
;;   (defun custom/evil-cp-modify-regular-bindings (&rest r)
;;     (setq evil-cp-regular-bindings
;;           (remove-if (lambda (key-string)
;;                        (member key-string '("_" ">" "<")))
;;                      evil-cp-regular-bindings
;;                      :key 'car)))
;;   (advice-add 'evil-cp--enable-regular-bindings :before
;;               #'custom/evil-cp-modify-regular-bindings)
;;   (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode))

;; (use-package evil-smartparens
;;   :commands (evil-smartparens-mode)
;;   :init
;;   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-symbol
  :commands (highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay .3)
  (setq highlight-symbol-highlight-single-occurrence nil))

(use-package olivetti)

(use-package clojure-mode
  :init
  ;; TODO: factor this out to general lisp mode
  (evil-define-key 'normal clojure-mode-map
    ">" 'evil-cp->
    "<" 'evil-cp-<
    "_" 'evil-cp-first-non-blank-non-opening
    "H" 'evil-cp-drag-backward
    "L" 'evil-cp-drag-forward))

(use-package alchemist
  :config
  (evil-define-key 'normal elixir-mode-map
    (kbd "C-]") 'alchemist-goto-definition-at-point))

(use-package emmet-mode
  :init
  (add-hook 'html-mode-hook #'emmet-mode)
  :config
  (evil-define-key nil evil-insert-state-map
    (kbd "C-y") 'emmet-expand-line))

(use-package which-key
  :config
  (which-key-mode))


(use-package avy
  :init
  (setq avy-style 'at-full)
  (setq avy-all-windows nil)
  (setq avy-background t)
  :config
  (defun avy-goto-word-in-line-0 ()
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position)))
  (defun avy-goto-line-word-0 ()
    (interactive)
    (avy-goto-line 1)
    (avy-goto-word-in-line-0))
  (evil-define-key nil evil-normal-state-map
    "gl" 'avy-goto-line-word-0
    "gw" 'avy-goto-word-1))

(use-package rtags
  :config
  (setq rtags-socket-file "/run/user/1000/rdm.socket")
  (setq rtags-completions-enabled t)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (evil-define-key 'normal c-mode-map
    (kbd "C-]") 'rtags-find-symbol-at-point
    (kbd "C-t") 'rtags-location-stack-back))
(add-hook 'c-mode-hook 'flycheck-mode)

(use-package flycheck-rtags)
(use-package company-rtags
  :init
  (eval-after-load 'company
     '(add-to-list 'company-backends 'company-rtags)))

;; (use-package irony
;;   :init
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   (add-hook 'irony-mode-hook #'irony-eldoc)
;;   (add-hook 'irony-mode-hook 'flycheck-mode)
;;   (eval-after-load 'company
;;      '(add-to-list 'company-backends 'company-irony)))
;;   :config
;;   (evil-leader/set-key
;;     "dc" (lambda ()
;;            (interactive)
;;            (manual-entry (current-word)))))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (evil-define-key 'normal go-mode-map
    (kbd "C-]") 'godef-jump)
  (evil-leader/set-key
    "dc" 'godoc-at-point))
(use-package company-go
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode))))
(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
  (setq-default nasm-basic-offset 4))


(use-package tide
  :ensure t
  :config
  (evil-define-key 'normal tide-mode-map
    (kbd "C-]") 'tide-jump-to-definition
    (kbd "C-t") 'tide-jump-back)
  (evil-leader/set-key
    "tf" 'tide-fix)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . company-mode)
         (typescript-mode . flycheck-mode)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . add-node-modules-path)))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :hook ((web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (tide-setup))))
         (web-mode . flycheck-mode)))

;; Deals with race condition between flycheck checking stuff and creating a file
;; which webpack sees and tries to process
;; which flycheck then deletes causing an error in webpack
;; (eval-after-load 'flycheck
;;   '(setcar (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command)) 'source-original))


(ensure-package-installed
 'company
 'elpy
 'editorconfig
 'evil-leader
 'evil-matchit
 'evil-nerd-commenter
 'evil-surround
 'flycheck
 'flycheck-irony
 'flycheck-rust
 'helm
 'highlight-symbol
 'projectile
 'geiser
 'irony
 'irony-eldoc
 'company-irony
 'rust-mode

 'racer
 'slime
 'slime-company
 'smooth-scrolling
 'helm-projectile
 ;; 'fill-column-indicator

 'rjsx-mode

 'add-node-modules-path

 'key-chord
 'navigate
 'rainbow-mode
 'yasnippet
 'yasnippet-snippets)

(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(smooth-scrolling-mode)
(setq smooth-scroll-margin 5)
; 20MB memory before calling GC
(setq gc-cons-threshold 20000000)
; store backups/auto-saves to /tmp
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-save-default nil)
; treat camelCase words as separate words
(add-hook 'prog-mode-hook 'subword-mode)
; always follow symlinks
(setq vc-follow-symlinks t)
; When saving a file in a directory that doesn't exist, offer
; to (recursively) create the file's parent directories
(add-hook 'before-save-hook
  (lambda ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t))))))
; human readable sizes in dired
(setq-default dired-listing-switches "-alh")
; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
; auto-refresh buffer when file changes
(global-auto-revert-mode t)

;; Causes auto indent to only do one 'indent' on 'add-hook'
(put 'add-hook 'lisp-indent-function 1)

;; (global-linum-mode t)
(column-number-mode)
(setq linum-format "%3d\u2502")

(setq whitespace-line-column 80)

(setq whitespace-style '(face lines-tail tabs trailing tab-mark))
(add-hook 'prog-mode-hook
  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . (lambda ()
                              (whitespace-mode t))))
;; (global-whitespace-mode 1)

;; (add-hook 'after-change-major-mode-hook 'fci-mode)
;; (setq-default fill-column 80)
                                        ; (global-fci-mode-1)

                                        ; write directory after buffer name
(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))


;; 4 space indenting in c
(setq-default c-basic-offset 4)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "C-s") 'save-all)

;; Testing using smartparens for this
;; to allow detection of user controlled pairs
;; (show-paren-mode t)
;; (setq show-paren-delay 0)

(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(global-company-mode)
;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evil mode config
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-want-C-u-scroll t)

(add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode +1)))
(add-hook 'evil-insert-state-exit-hook (lambda () (hl-line-mode -1)))

; need to be defined before evil and sometimes breaks other
; evil things, so put it RIGHT before evil
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "h" 'help
  "s" 'save-all
  "p" 'helm-projectile-switch-project
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "ft" 'evil-toggle-fold
  "fo" 'evil-open-folds
  "fc" 'evil-close-folds)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(define-key evil-insert-state-map
  (kbd "C-s")
  (lambda ()
    (interactive)
    (save-all)
    (evil-normal-state)))

(define-key evil-normal-state-map
  (kbd "Y")
  (lambda ()
    (interactive)
    (evil-yank (point) (point-at-eol))))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;; (setq evil-emacs-state-modes nil)

; allow RET/space to be useable in other modes even when in evil mode
(defun custom-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(custom-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(custom-move-key evil-motion-state-map evil-normal-state-map " ")

(require 'navigate)

(require 'evil-surround)
(global-evil-surround-mode 1)

(global-set-key (kbd "C-p") 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
(define-key evil-visual-state-map (kbd "C-p") 'helm-projectile-find-file)


(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defalias 'Ex 'dired)
(defalias 'ex 'dired)

(add-hook 'dired-mode-hook
  (lambda ()
    (auto-revert-mode)
    ;; (define-key dired-mode-map (kbd "C-p") 'helm-projectile-find-file)
    (define-key evil-normal-state-local-map (kbd "G") 'evil-goto-line)
    (define-key evil-normal-state-local-map (kbd "?") 'evil-search-backward)
    (define-key evil-normal-state-local-map (kbd "n") 'evil-search-next)
    (define-key evil-normal-state-local-map (kbd "N") 'evil-search-previous)
    (define-key evil-normal-state-local-map (kbd "-") 'dired-up-directory)
    (define-key evil-normal-state-local-map (kbd "d") 'dired-create-directory)
    (define-key evil-normal-state-local-map (kbd "%") 'find-file)))

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; there are some expansion conflicts I don't feel like dealing with
(require 'elpy)
(delete 'elpy-module-yasnippet elpy-modules)
(elpy-enable)
(add-hook 'elpy-mode-hook
  (lambda ()
    (highlight-indentation-mode 0)))


;; (defun buffer-local-tab-complete ()
;;   "Make `tab-always-indent' a buffer-local variable and set it to 'complete."
;;   (make-local-variable 'tab-always-indent)
;;     (setq tab-always-indent 'complete))

(setq geiser-active-implementations '(mit))
(add-hook 'geiser-mode-hook
  (lambda ()
    (evil-cleverparens-mode)
    (geiser-smart-tab-mode)))


(defun slime-eval-last-defun-in-repl (prefix)
    (interactive "P")
    (let ((expr (slime-defun-at-point))
          (buffer-name (buffer-name (current-buffer)))
          (new-package (slime-current-package))
          (old-package (slime-lisp-package))
          (slime-repl-suppress-prompt t)
          (yank-back nil))
      (with-current-buffer (slime-output-buffer)
        (unless (eq (current-buffer) (window-buffer))
          (pop-to-buffer (current-buffer) t))
        (goto-char (point-max))
        ;; Kill pending input in the REPL
        (when (< (marker-position slime-repl-input-start-mark) (point))
          (kill-region slime-repl-input-start-mark (point))
          (setq yank-back t))
        (unwind-protect
            (progn
              (insert-before-markers (format "\n;;; from %s\n" buffer-name))
              (when new-package
                (slime-repl-set-package new-package))
              (let ((slime-repl-suppress-prompt nil))
                (slime-repl-insert-prompt))
              (insert expr)
              (slime-repl-return))
          (unless (or t (equal (slime-lisp-package) old-package))
            ;; Switch back.
            (slime-repl-set-package old-package)
            (let ((slime-repl-suppress-prompt nil))
              (slime-repl-insert-prompt))))
        ;; Put pending input back.
        (when yank-back
                  (yank)))))

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-fancy slime-company))

(setq slime-eval-comment-fmt ";=> ")

(defun delete-eval-comments ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (flush-lines (concat "^" slime-eval-comment-fmt))))

(defun on-eval-comment-line ()
  (save-excursion
    (search-forward slime-eval-comment-fmt (+ (point) (length slime-eval-comment-fmt)) 0)))

(defun insert-lisp-comment-on-new-line (comment)
  (when (on-eval-comment-line) (kill-whole-line))
  (newline)
  (previous-line)
  (insert slime-eval-comment-fmt)
  (insert comment))

(defun slime-eval-print-sexp ()
  (interactive)
  (setq comment (cadr (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))))
  (save-excursion
    (end-of-defun)
    (insert-lisp-comment-on-new-line comment)))

(defun slime-eval-print-defun ()
  (interactive)
  (setq comment (cadr (slime-eval `(swank:eval-and-grab-output ,(slime-defun-at-point)))))
  (save-excursion
    (end-of-defun)
    (insert-lisp-comment-on-new-line comment)))

(defun eval-print-all-defun ()
  (unless (eq (point) (point-max))
    (slime-eval-print-defun)
    (end-of-defun)
    (next-line)
    (eval-print-all-defun)))

(defun slime-eval-print-file ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (eval-print-all-defun)))

(add-hook 'slime-mode-hook
  (lambda ()
    (evil-cleverparens-mode)
    (rainbow-delimiters-mode)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem)
    (evil-leader/set-key
      "cd" 'slime-eval-print-defun
      "cs" 'slime-eval-print-sexp
      "cf" 'slime-eval-print-file
      "dc" 'delete-eval-comments
      "rs" 'slime-eval-last-expression-in-repl
      "rd" 'slime-eval-last-defun-in-repl
      "bd" 'slime-eval-defun
      "bs" 'slime-eval-last-expression
      )))

;; insert matches instead of requiring them to be selected
(setq company-frontends
    '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend company-echo-metadata-frontend))
;; (defun on-off-fci-before-company(command)
;;   (when (string= "show" command)
;;     (turn-off-fci-mode))
;;   (when (string= "hide" command)
;;     (turn-on-fci-mode)))
;; (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(add-hook 'rust-mode-hook
  (lambda ()
    (racer-mode)
    (flycheck-mode)
    (setq rust-format-on-save t)
    (evil-define-key 'normal rust-mode-map
      "gd" 'racer-find-definition)
    (evil-leader/set-key
      "dc" 'racer-describe)))

(add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "C-b") 'company-complete-selection)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#4f57f9" :foreground "white"))))
 '(flymake-error ((((class color)) (:background "Gray27"))))
 '(flymake-warning ((((class color)) (:background "Gray27"))))
 '(highlight-symbol-face ((t (:background "color-19"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow")))))
;; '(linum ((t (:background "#282a2e" :foreground "#969896"))))


(evil-leader/set-key
  "fn" 'flymake-goto-next-error
  "fp" 'flymake-goto-prev-error)

(setenv "MANWIDTH" "80")
(evil-set-initial-state 'Man-mode 'normal)

;; Prevent irony mode from enabling in 'derived' modes
;; (defun my-irony-enable ()
;;   (when (member major-mode irony-supported-major-modes)
;;     (irony-mode 1)))

;; (add-hook 'c++-mode-hook 'my-irony-enable)
;; (add-hook 'c-mode-hook 'my-irony-enable)

;; (add-hook 'objc-mode-hook 'irony-mode)


(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'flycheck-mode-hook
  (lambda ()
    (evil-leader/set-key
      "fn" 'flycheck-next-error
      "fp" 'flycheck-previous-error
      "fl" 'flycheck-list-errors)))
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-odin-setup))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-clang-unity-setup))

;; (require 'midnight)
;; (setq midnight-period (* 60 60))

;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; (setq web-mode-enable-auto-closing t)
;; (setq web-mode-enable-auto-pairing t)


(add-hook 'rjsx-mode-hook
  (lambda ()
    (setq-default js2-basic-offset 2)
    (setq-default js-indent-level 2)
    (setq-default sgml-attribute-offset 0)
    (define-key evil-insert-state-map (kbd "C-d") nil)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(setq-default js2-strict-trailing-comma-warning nil)

(defun indent-close-tag-with-open (func &rest args)
  (apply func args)
  (save-excursion
    (let
        ((current-line (string-trim-left(buffer-substring
                                         (line-beginning-position)
                                         (line-end-position)))))
      (if (string-equal (substring current-line 0 1) ">")
          (progn
            (goto-char (line-beginning-position))
            (search-forward ">")
            (goto-char (- (point) 1))
            (delete-backward-char 2))))))
(advice-add 'sgml-indent-line :around #'indent-close-tag-with-open)

;; (defvar js-jsx-tag-syntax-table
;;   (let ((table (make-syntax-table sgml-tag-syntax-table)))
;;     (modify-syntax-entry ?\{ "<" table)
;;     (modify-syntax-entry ?\} ">" table)
;;     table))

;; (defun advice-js-jsx-indent-line (orig-fun)
;;   (interactive)
;;   (let ((sgml-tag-syntax-table js-jsx-tag-syntax-table))
;;     (apply orig-fun nil)))

;; (advice-add 'js-jsx-indent-line :around 'advice-js-jsx-indent-line)
