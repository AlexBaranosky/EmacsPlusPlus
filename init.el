(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(load (expand-file-name "~/.emacs.d/core.el"))
(load (expand-file-name "~/.emacs.d/el-get-sources.el"))
(load (expand-file-name "~/.emacs.d/extensions.el"))
(when (file-exists-p "~/.emacs.d/userspecific.el")
  (load (expand-file-name "~/.emacs.d/userspecific.el")))
(when (file-exists-p "~/.emacs.d/user.el")
  (load (expand-file-name "~/.emacs.d/user.el")))
