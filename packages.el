(when window-system
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t))

(add-to-list 'load-path "~/.emacs.d")
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;

(defvar packages '(ac-nrepl
                   ace-jump-mode
                   auto-complete
                   cc-mode
                   cider
                   cl
                   clj-refactor
                   clojure-mode
                   clojure-test-mode
                   col-highlight
                   compile
                   crosshairs
                   dash
                   etags-select
                   etags-table
                   expand-region
                   fill-column-indicator
                   fuzzy
                   fuzzy-match
                   ghc
                   gist
                   go-mode
                   guru-mode
                   haskell-mode
                   highlight-parentheses
                   highlight-symbol
                   highline
                   hl-sexp
                   idle-highlight
                   ido-hacks
                   idomenu
                   jabber
                   key-chord
                   markdown-mode
                   mic-paren
                   multiple-cursors
                   nav
                   projectile
                   protobuf-mode
                   s
                   saveplace
                   scala-mode
                   ;; scion-mode ;; TODO...
                   slamhound
                   starter-kit
                   starter-kit-bindings
                   starter-kit-eshell
                   starter-kit-lisp
                   subword
                   undo-tree
                   winner
                   yasnippet

                   ;; color-theme-sanityinc-solarized
                   ;; js2-mode
                   ;; js2-refactor
                   ;; clues-theme
                   ;; cyberpunk-theme
                   ;; deep-thought-theme
                   ;; twilight-theme
                   ;; zenburn-theme
                   ))

(dolist (pkg packages)
  (package-require pkg))
