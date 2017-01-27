(require 'package)
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
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(package-initialize)

(ensure-package-installed
  'ack
  'base16-theme
  'company
  'elpy
  'editorconfig
  'evil
  'evil-leader
  'evil-matchit
  'evil-nerd-commenter
  'evil-surround
  'evil-cleverparens
  'helm
  'rainbow-delimiters
  ;; 'paredit
  'projectile
  'geiser
  ;; 'racket-mode
  'slime
  'slime-company
  'smooth-scrolling
  'helm-projectile
  'fill-column-indicator
  'key-chord
  'navigate
  'rainbow-mode
  'yasnippet)

(menu-bar-mode -1)
(setq column-number-mode t)
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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

(show-paren-mode t)
(setq show-paren-delay 0)

(global-linum-mode t)
(setq linum-format "%3d\u2502")

(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq-default fill-column 80)
; (global-fci-mode-1)

; write directory after buffer name
(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                                                            (cdr mode-line-buffer-identification)))))

(load-theme 'base16-tomorrow-night t)
; this renders incorrect in terminal vim
(add-to-list 'default-frame-alist '(background-color . "black"))

; 4 space indenting in c
(setq-default c-basic-offset 4)

(require 'editorconfig)
(editorconfig-mode 1)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "C-s") 'save-all)

(require 'yasnippet)
(yas-global-mode 1)

(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Evil mode config
;
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
  "p" 'helm-projectile-switch-project
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

; order is important
(require 'evil)
(evil-mode 1)

(define-key evil-insert-state-map
  (kbd "C-s")
  (lambda ()
    (interactive)
    (save-all)
    (evil-normal-state)))

(define-key evil-normal-state-map (kbd "K") nil)
(define-key evil-visual-state-map (kbd "K") nil)

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

(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'navigate)

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
    (define-key evil-normal-state-local-map (kbd "-") 'dired-up-directory)
    (define-key evil-normal-state-local-map (kbd "d") 'dired-create-directory)
    (define-key evil-normal-state-local-map (kbd "%") 'find-file)))

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(elpy-enable)
(add-hook 'elpy-mode-hook
    (lambda ()
      (highlight-indentation-mode 0)))


(defun buffer-local-tab-complete ()
  "Make `tab-always-indent' a buffer-local variable and set it to 'complete."
  (make-local-variable 'tab-always-indent)
    (setq tab-always-indent 'complete))

;; (add-hook 'racket-mode-hook
;;   (lambda ()
;;     ;; (paredit-mode)
;;     (smartparens-mode)
;;     (evil-cleverparens-mode)
;;     (buffer-local-tab-complete)
;;     (evil-leader/set-key
;;       "r" 'racket-run)))

(setq geiser-active-implementations '(mit))
(add-hook 'geiser-mode-hook
  (lambda ()
    ;; (paredit-mode)
    (smartparens-mode)
    (evil-cleverparens-mode)
    (geiser-smart-tab-mode)
    ;; (evil-leader/set-key
    ;;   "r" 'racket-run))
    ))


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
    (smartparens-mode)
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
(defun on-off-fci-before-company(command)
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))
(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     ))

(custom-set-faces
    '(flymake-errline ((((class color)) (:background "Gray30"))))
    '(flymake-warnline ((((class color)) (:background "Gray30")))))

(evil-leader/set-key
    "fn" 'flymake-goto-next-error
    "fp" 'flymake-goto-prev-error)


(defun my-ack-default-directory (_arg)
  (or (ack-guess-project-root default-directory)
      (read-directory-name "In directory: " nil nil t)))

(setq ack-default-directory-function #'my-ack-default-directory)
