(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
(add-hook 'nrepl-connected-hook
          (defun pnh-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode) ;;?
            (cider-enable-on-existing-clojure-buffers)))
(add-hook 'cider-mode-hook 'subword-mode)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(setq clojure-font-lock-comment-sexp t)



(defun cider-p-eval-last-sexp ()
  "Evaluate the expression preceding point and `p` its value in a popup buffer."
  (interactive)
  (let ((form (cider-last-sexp))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (format "(gui.diff/p %s)" form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(defun cider-eval++ (str)
  (cider-eval-sync str (cider-current-ns)))

(defun alembic ()
  (interactive)
  (cider-eval++ "(require '[alembic.still :as alembic])
                 (alembic/load-project)"))

(defun refresh ()
  (interactive)
  (cider-eval++ "(require '[clojure.tools.namespace.repl :refer [refresh]])
                 (refresh)"))

(defun gui-diff ()
  (interactive)
  (cider-eval++ "(require '[gui.diff :refer :all])"))

(defun refresh-nrepl ()
  (interactive)
  (refresh)
  (gui-diff))

(define-key cider-mode-map (kbd "C-c C-q") 'cider-p-eval-last-sexp)
(define-key cider-mode-map (kbd "C-c C-a") 'alembic)
(define-key cider-mode-map (kbd "C-c C-f") 'refresh-nrepl)
