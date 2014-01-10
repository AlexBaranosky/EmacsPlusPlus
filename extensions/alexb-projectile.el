(defun projectile-cleanup-project-buffers ()
  (interactive)
  (dolist (buffer (projectile-project-buffer-names))
    (condition-case nil
        (with-current-buffer buffer
          (esk-cleanup-buffer))
        (buffer-read-only nil))))

(defun projectile-current-project-file-full-paths ()
  (let ((root (projectile-project-root)))
    (mapcar (lambda (filename)
              (expand-file-name filename root))
            (projectile-current-project-files))))

(defun projectile-cleanup-project-files ()
  (interactive)
  (dolist (filename (projectile-current-project-file-full-paths))
    (cleanup-file filename)))

(global-set-key (kbd "C-c N") 'projectile-cleanup-project-buffers)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)

(provide 'alexb-projectile)
