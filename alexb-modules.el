(defun load-module (filename)
  (load (expand-file-name filename "~/.emacs.d/modules")))

(load-module "alexb-cider-mode.el")
(load-module "alexb-clojure.el")
(load-module "alexb-erc.el")
(load-module "alexb-magit.el")
(load-module "alexb-paredit.el")

(load-module "alexb-catchall.el")

