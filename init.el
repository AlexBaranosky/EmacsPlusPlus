(load (expand-file-name "~/.emacs.d/alexb-core.el"))

(defun setup-cider ()
  (alexb-package-require 'cider)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
  (add-hook 'nrepl-connected-hook
            (defun pnh-clojure-mode-eldoc-hook ()
              (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
              (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode) ;;?
              (cider-enable-on-existing-clojure-buffers)))
  (add-hook 'cider-mode-hook 'subword-mode)
  (alexb-package-require 'ac-nrepl)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode))
  (add-hook 'cider-mode-hook 'ac-nrepl-setup))

(defun setup-clojure-packages ()
  (alexb-package-require 'clojure-mode)
  (setup-cider)
  (alexb-package-require 'clojure-test-mode)
  (add-hook 'clojure-mode 'paredit-mode)
  (add-hook 'clojure-mode 'clojure-test-mode))

(defun setup-starter-kits-packages ()
  (alexb-package-require 'starter-kit)
  (alexb-package-require 'starter-kit-bindings)
  (alexb-package-require 'starter-kit-eshell)
  (alexb-package-require 'starter-kit-lisp))

(defun setup-remaining-packages ()
  (alexb-package-require 'ace-jump-mode)
  (alexb-package-require 'all)
  (alexb-package-require 'alpha)
  (alexb-package-require 'cc-mode)
  (alexb-package-require 'cl)
  (alexb-package-require 'col-highlight)
  (alexb-package-require 'compile)
  ;; (alexb-package-require 'color-theme-sanityinc-solarized)

  (alexb-package-require 'crosshairs)
  (alexb-package-require 'dash)
  (alexb-package-require 'etags-select)
  (alexb-package-require 'etags-table)
  (alexb-package-require 'fill-column-indicator)
  (alexb-package-require 'fuzzy)
  (alexb-package-require 'fuzzy-match)
  (alexb-package-require 'go-mode)
  (alexb-package-require 'haskell-mode)
  (alexb-package-require 'highlight-parentheses)
  (alexb-package-require 'highlight-symbol)
  (alexb-package-require 'highline)
  (alexb-package-require 'hl-sexp)
  (alexb-package-require 'idle-highlight)
  (alexb-package-require 'rainbow-delimiters)
  (alexb-package-require 'markdown-mode)
  (alexb-package-require 'maxframe)
  (alexb-package-require 'mic-paren)
  (alexb-package-require 'nav)
  (alexb-package-require 's)
  (alexb-package-require 'scala-mode)
  (alexb-package-require 'slamhound)
  (alexb-package-require 'projectile)
  (alexb-package-require 'protobuf-mode)
  ;; (alexb-package-require 'clues-theme)
  ;;(alexb-package-require 'cyberpunk-theme)
  ;; (alexb-package-require 'deep-thought-theme)
  ;;(alexb-package-require 'twilight-theme)
  (alexb-package-require 'vline)
  ;; (alexb-package-require 'zenburn-theme)
)

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))



;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" magit-diff-options)
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (add-to-list 'magit-diff-options "-w")
;;   (magit-refresh))

;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (setq magit-diff-options (remove "-w" magit-diff-options))
;;   (magit-refresh))

;;;; Load 'Em Up!

(setup-starter-kits-packages)
(setup-clojure-packages)
(setup-remaining-packages)

;; (define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)
(setq magit-highlight-whitespace nil)

(define-key paredit-mode-map (kbd "M-)")
  'paredit-wrap-round-from-behind)
;; (key-chord-define-global "dp" 'paredit-duplicate-closest-sexp)


(defun cider-p-eval-last-sexp ()
  "Evaluate the expression preceding point and `p` its value in a popup buffer."
  (interactive)
  (let ((form (cider-last-sexp))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (format "(gui.diff/p %s)" form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(define-key cider-mode-map (kbd "C-c C-q") 'cider-p-eval-last-sexp)


(load (expand-file-name "~/.emacs.d/alexb-modules.el"))


;;;; Emacs Auto-Added Code, below

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
