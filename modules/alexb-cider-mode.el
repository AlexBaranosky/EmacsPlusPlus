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

(define-key cider-mode-map (kbd "C-c C-q") 'cider-p-eval-last-sexp)

