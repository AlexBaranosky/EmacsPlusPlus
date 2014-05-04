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

(defun cljr-add-dev-ns (ns)
  (setq cljr-auto-sort-ns nil)
  (ignore-errors
    (cljr-add-require-to-ns)
    (insert ns)
    (yas-exit-all-snippets)
    (cljr-sort-ns))
  (setq cljr-auto-sort-ns t))

(defun projectile-cleanup-project-x-files
  (dev-ns-str)
  (dolist (filename (projectile-current-project-file-full-paths))
    (when (s-ends-with? "clj" filename)
      (ignore-errors
        (find-file filename)
        (cljr-add-dev-ns dev-ns-str)
        (cljr-remove-unused-requires)
        (cleanup-file filename)))))

(defun projectile-cleanup-project-furtive-files ()
  (interactive)
  (projectile-cleanup-project-x-files "[furtive.dev :refer :all]"))

(defun projectile-cleanup-project-drive-files ()
  (interactive)
  (projectile-cleanup-project-x-files "[drive.dev :refer :all]"))

(defun projectile-cleanup-project-slarti-files ()
  (interactive)
  (projectile-cleanup-project-x-files "[slartibartfast.dev :refer :all]"))

(defun projectile-cleanup-project-clj-files ()
  (interactive)
  (dolist (filename (projectile-current-project-file-full-paths))
    (when (s-ends-with? "clj" filename)
      (ignore-errors
        (find-file filename)
        (cljr-remove-unused-requires)
        (cljr-sort-ns)
        (cleanup-file filename)))))


(global-set-key (kbd "C-c N") 'projectile-cleanup-project-buffers)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)

(provide 'emacs++-projectile)
