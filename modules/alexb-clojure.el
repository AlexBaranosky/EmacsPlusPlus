(add-hook 'clojure-mode 'paredit-mode)
(add-hook 'clojure-mode 'clojure-test-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-t")))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

