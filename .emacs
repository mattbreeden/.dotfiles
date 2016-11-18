(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

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
  'base16-theme
  'evil
  'evil-leader
  'evil-matchit
  'evil-nerd-commenter
  'evil-surround
  'helm
  'projectile
  'helm-projectile
  'fiplr
  'fill-column-indicator
  'key-chord
  'navigate)

(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq scroll-conservatively 1)
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

(linum-mode)
(setq linum-format "%2d\u2502")
(ensure-package-installed 'linum-relative)
(setq linum-relative-format "%3s\u2502")
(setq linum-relative-current-symbol "")
(linum-relative-global-mode)

(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq-default fill-column 80)
; (global-fci-mode-1)

; (load-theme 'base16-tomorrow-night t)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "C-s") 'save-all)

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
  "f" 'fiplr-find-file
  "e" 'dired
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
  (kbd "C-s") (lambda () (interactive)
    (save-all)
    (evil-normal-state)))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

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
    ;; (define-key dired-mode-map (kbd "C-p") 'helm-projectile-find-file)
    (define-key evil-normal-state-local-map (kbd "-") 'dired-up-directory)
    (define-key evil-normal-state-local-map (kbd "d") 'dired-create-directory)
    (define-key evil-normal-state-local-map (kbd "%") 'find-file)))

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fiplr navigate linum-relative key-chord helm-projectile fill-column-indicator evil-surround evil-nerd-commenter evil-matchit evil-leader base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
