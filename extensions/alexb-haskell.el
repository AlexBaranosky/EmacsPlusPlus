(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)


;; Pick *one* of these three:
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda ()
                               (ghc-init)
                               (flymake-mode)))

(setq haskell-program-name "ghci")
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))
(setq inferior-haskell-find-project-root nil)

;;;; Scion ;; TODO...

;; ;; Substitute the desired version for <version>
;; (add-to-list 'load-path "~/.cabal/share/scion-<version>/emacs")
;; (require 'scion)

;; ;; if ./cabal/bin is not in your $PATH
;; (setq scion-program "~/.cabal/bin/scion-server")

;; (add-hook 'haskell-mode-hook (lambda ()
;;                                ;; Whenever we open a file in Haskell mode, also
;;                                ;; activate Scion
;;                                (scion-mode 1)
;;                                ;; Whenever a file is saved, immediately type
;;                                ;; check it and highlight errors/warnings in the
;;                                ;; source.
;;                                (scion-flycheck-on-save 1)))

;; ;; Use ido-mode completion (matches anywhere, not just beginning)
;; ;;
;; ;; WARNING: This causes some versions of Emacs to fail so badly
;; ;; that Emacs needs to be restarted.
;; (setq scion-completing-read-function 'ido-completing-read)

(provide 'alexb-haskell)
