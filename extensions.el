(defun load-extension (filename)
  (load (expand-file-name filename "~/.emacs.d/extensions")))

(load-extension "alexb-cider-mode.el")
(load-extension "alexb-clojure.el")
(load-extension "alexb-erc.el")
(load-extension "alexb-haskell.el")
(load-extension "alexb-magit.el")
(load-extension "alexb-paredit.el")

(load-extension "alexb-catchall.el")

