(defun load-module (filename)
  (load (expand-file-name filename "~/.emacs.d/modules")))

(load-module "alexb-erc.el")

