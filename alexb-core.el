(defun alexb--setup-packaging-system ()
  (when window-system
    (require 'package))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (add-to-list 'load-path "~/.emacs.d")
  (add-to-list 'load-path "~/.emacs.d/modules/alexb-erc.el")
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents)))

(alexb--setup-packaging-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-refresh-first-time nil)
(defun alexb-package-require (package-name)
  "tries to require. If it fails, it retrieves the package and
   tries to require again (y benzap on #emacs)"
  (if (not (require package-name nil t))
      (progn
        (if (not package-refresh-first-time)
            (save-excursion
              (package-refresh-contents)
              (setq package-refresh-contents t)))
        (package-install package-name)
        (require package-name nil t))))

(defmacro alexb-rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after alexb-rename-modeline activate)
        (setq mode-name ,new-name))))

(defun alexb-rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun alexb-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
;; (add-hook 'before-save-hook 'alexb-cleanup-buffer-safe)

(defun alexb-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (alexb-cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun alexb-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun alexb-goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun alexb-cursor-jump-up ()
  (interactive)
  (forward-line -15))

(defun alexb-cursor-jump-down ()
  (interactive)
  (forward-line 15))

(defun alexb-switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(alexb-package-require 'key-chord)
(key-chord-mode 1)

(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(alexb-package-require 'expand-region)

(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jc" 'ace-jump-char-mode)
(key-chord-define-global "gg" 'goto-line)
(key-chord-define-global "zz" 'repeat)
(key-chord-define-global "--" 'alexb-alexb-switch-to-previous-buffer)
(key-chord-define-global "gp" 'rgrep)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "ii" 'alexb-cursor-jump-up)
(key-chord-define-global "kk" 'alexb-cursor-jump-down)  

(global-set-key (kbd "C-c C-r") 'rename-sgml-tag)

(alexb-package-require 'auto-complete)
(global-auto-complete-mode t)

(alexb-package-require 'jabber)
(setq jabber-nickname "Alex Baranosky")
(setq jabber-account-list
      '(("alexander.baranosky@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))

(alexb-package-require 'subword)
(subword-mode)

(alexb-package-require 'winner)
(winner-mode)

(alexb-package-require 'textmate)
(global-set-key (kbd "C-x M-f") 'textmate-goto-file)

(alexb-package-require 'idomenu)
(defvar push-mark-before-goto-char nil)
(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))
(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (idomenu)))
(global-set-key (kbd "C-x C-i") 'idomenu)

(alexb-package-require 'multiple-cursors)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(alexb-package-require 'yasnippet)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt))
(yas/global-mode 1)
(yas/load-directory "~/.emacs.d/snippets/clojure-mode")
(yas/load-directory "~/.emacs.d/snippets/org-mode")
(key-chord-define-global "yy" 'yas-insert-snippet)

;; (alexb-package-require 'js2-mode)
;; (alexb-package-require 'js2-refactor)
;; (js2r-add-keybindings-with-prefix "C-c C-t")

(alexb-package-require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-t")))

(show-paren-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(alexb-package-require 'ido-hacks)

(alexb-package-require 'undo-tree)
(global-undo-tree-mode)


(global-set-key [remap goto-line] 'alexb-goto-line-with-feedback)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x C-r") 'alexb-rename-current-buffer-file)
(global-set-key (kbd "C-c n") 'alexb-cleanup-buffer)


;; Save point position between sessions
(alexb-package-require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(alexb-rename-modeline "js2-mode" js2-mode "JS2")
(alexb-rename-modeline "clojure-mode" clojure-mode "CLJ")

(put 'upcase-region 'disabled nil)
  
(add-to-list
 'auto-mode-alist
 '("\\.edn\\'" . clojure-mode))
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list
 'auto-mode-alist
 '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(when (file-exists-p "~/.zshrc")
  ;; Get zshrc into PATH
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(global-auto-revert-mode t)
;; (global-linum-mode)
(setq clojure-font-lock-comment-sexp t)
(setq-default fill-column 80)
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (set-default-font "-apple-inconsolata-medium-r-normal--14-180-72-72-m-180-iso8859-1"))


(setq ring-bell-function (lambda () (message "*beep*")))
;; (load-theme 'zenburn)
(load-theme 'sanityinc-solarized-dark)
;; (load-theme 'twilight-theme)
;; (load-theme 'clues)
;;(load-theme 'cyberpunk)
;; (load-theme 'deep-thought)
