
(setq package-refresh-first-time nil)
(defun package-require (package-name)
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

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (if (not (> (count-windows)1))
      (message "You can't rotate a single window!")
    (progn
      (setq i 1)
      (setq numWindows (count-windows))
      (while (< i numWindows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i numWindows) 1)))

               (b1 (window-buffer w1))
               (b2 (window-buffer w2))

               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i)))))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
   Does not indent buffer, because it is used for a before-save-hook, and that
   might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

;; (add-hook 'prog-mode (lambda ()
;;                        (add-hook 'before-save 'cleanup-buffer nil t)))

;; TODO: delete if the above works
;; Various superfluous white-space. Just say no.
;; (add-hook 'before-save cleanup-buffer-safe)


;; (defun cleanup-file (filename)
;;   (interactive "sFile: ")
;;   (find-file filename)
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (error "Buffer '%s' is not visiting a file!" name)
;;       (progn
;;         (esk-cleanup-buffer)
;;         (save-buffer)))))

(defun rename-current-buffer-file ()
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

(defun sudo-find-file (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun cursor-jump-up ()
  (interactive)
  (forward-line -15))

(defun cursor-jump-down ()
  (interactive)
  (forward-line 15))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun save-macro (name)
  "save a macro. Take a name as argument
   and save the last defined macro under
   this name at the end of your .emacs"
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file "~/.emacs.d/core.el") ;; user-init-file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (switch-to-buffer nil))

(defun emacs++-insert-date ()
  (interactive)
  (insert (format-time-string "%a, %b %e, %Y")))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; (defun dired-back-to-top ()
;;   (interactive)
;;   (beginning-of-buffer)
;;   (dired-next-line 4))

;; (defun dired-jump-to-bottom ()
;;   (interactive)
;;   (end-of-buffer)
;;   (dired-next-line -1))

;; (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
;; (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)



;; Elisp go-to-definition with M-. and back again with M-,
;(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-c C-r") 'rename-sgml-tag)

(global-set-key [remap goto-line] 'goto-line)
;; (global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x f") 'sudo-find-file)

;; (global-set-key (kbd "C-?") 'help-command)
;; (global-set-key (kbd "M-?") 'mark-paragraph)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode t)))

(eval-after-load "webjump"
  '(progn
     (add-to-list 'webjump-sites
                  '("Hoogle" .
                    [simple-query
                     "http://www.haskell.org/hoogle/"
                     "http://www.haskell.org/hoogle/?hoogle="
                     ""]))
     (add-to-list 'webjump-sites
                  '("Clojure Symbols" .
                    [simple-query
                     "http://symbolhound.com"
                     "http://symbolhound.com/?q=clojure+"
                     ""]))))

;; ;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size")
  (cond
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; ;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

(ad-activate 'ibuffer)

(put 'upcase-region 'disabled nil)

(when (file-exists-p "~/.zshrc")
  ;; Get zshrc into PATH
  (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
          (append
           (split-string-and-unquote path ":")
           exec-path))))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/extensions")

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/emacs.d/backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/emacs.d/backups") t)))

(winner-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode t)
(global-linum-mode)
(subword-mode 1)

(setq-default fill-column 80)
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'meta)
;;  (setq mac-command-modifier 'meta)
  (set-default-font "-apple-inconsolata-medium-r-normal--14-180-72-72-m-180-iso8859-1"))

(setq ring-bell-function (lambda () (message "*beep*")))
(setq custom-file "~/.emacs.d/custom.el")


;;; Auto-created fns from keyboard macros

(fset 'gui-diff-last-failure
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 97 99 116 117 97 108 58 13 134217734 19 40 61 13 right 201326624 201326624 134217847 134217790 40 103 117 105 45 100 105 102 102 32 25 41] 0 "%d")) arg)))

(fset 'split-let
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 112 97 return 93 91 backspace 91 left 32 40 108 101 116 backspace backspace backspace backspace 134217848 112 return 40 108 101 116 67108905 M-left left return 3 110] 0 "%d")) arg)))
