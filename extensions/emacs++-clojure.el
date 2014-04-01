(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(add-hook 'clojure-mode 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-x")))

(define-key clojure-mode-map (kbd "C->") 'cljr-thread)
(define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)
(define-key clojure-mode-map (kbd "M-C->") 'cljr-thread-first-all)
(define-key clojure-mode-map (kbd "M-C-?") 'cljr-thread-last-all)
;; (define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
;; (define-key clojure-mode-map (kbd "C-;") 'cljr-cycle-coll)
(key-chord-define-global "c[" 'cljr-cycle-coll)
(key-chord-define-global "i[" 'cljr-cycle-if)
(key-chord-define-global "p[" 'cljr-cycle-privacy)
(key-chord-define-global "s[" 'cljr-cycle-stringlike)

;; Warn about missing nREPL instead of doing stupid things

(defun emacs++-nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-z") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-k") 'emacs++-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-n") 'emacs++-nrepl-warn-when-not-connected)


(provide 'emacs++-clojure)
