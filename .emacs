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
  'key-chord
  'navigate)

(setq require-final-newline t)

(global-linum-mode t)
(setq linum-format "%3d\u2502")

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
