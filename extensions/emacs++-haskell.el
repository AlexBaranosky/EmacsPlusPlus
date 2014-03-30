(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


;; Pick *one* of these three:
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(after-load 'flycheck
  (package-require 'flycheck-hdevtools))

(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(add-auto-mode 'haskell-mode "\\.ghci\\'")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda ()
                               (ghc-init)
                               (flymake-mode)))

(setq haskell-program-name "ghci")
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))
(setq inferior-haskell-find-project-root nil)

(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

(provide 'emacs++-haskell)
