
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

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun rotate-windows ()
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

(defun projectile-cleanup-project-buffers ()
  (interactive)
  (dolist (buffer (projectile-project-buffer-names))
    (condition-case nil
        (with-current-buffer buffer
          (esk-cleanup-buffer))
      (buffer-read-only nil))))

(defun cleanup-file (filename)
  (interactive "sFile: ")
  (find-file filename)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (progn
        (esk-cleanup-buffer)
        (save-buffer)))))

(defun projectile-current-project-file-full-paths ()
  (let ((root (projectile-project-root)))
    (mapcar (lambda (filename)
              (expand-file-name filename root))
            (projectile-current-project-files))))

(defun projectile-cleanup-project-files ()
  (interactive)
  (dolist (filename (projectile-current-project-file-full-paths))
    (cleanup-file filename)))

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

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c N") 'projectile-cleanup-project-buffers)

(show-paren-mode 1)
(put 'upcase-region 'disabled nil)

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

(setq-default fill-column 80)
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (set-default-font "-apple-inconsolata-medium-r-normal--14-180-72-72-m-180-iso8859-1"))

(setq ring-bell-function (lambda () (message "*bell*")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
