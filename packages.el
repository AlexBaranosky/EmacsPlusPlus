(when window-system
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t))

(add-to-list 'load-path "~/.emacs.d")

(add-to-list 'load-path "~/.emacs.d/extensions")

(dolist (project (directory-files "~/.emacs.d/site-lisp" t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;

(defvar packages '(hl-line+ ;; comes first, others depend on it
                   ac-inf-ruby
                   ac-nrepl
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
                   derived
                   etags-select
                   etags-table
                   expand-region
                   fill-column-indicator
                   flycheck-hdevtools
                   fuzzy
                   fuzzy-match
                   ghc
                   ghci-completion
                   gist
                   go-mode
                   guru-mode
                   haskell-mode
                   highlight-parentheses
                   highlight-symbol
                   highline

                   hl-sexp
                   idle-highlight
                   iy-go-to-char
                   ;; ido-hacks
                   idomenu
                   inf-ruby
                   jabber
                   key-chord
                   markdown-mode
                   mic-paren
                   ;;; mmm-mode
                   multiple-cursors
                   ;; nav
                   projectile
                   protobuf-mode
                   robe
                   ruby-compilation
                   ruby-mode
                   ruby-hash-syntax
                   s
                   saveplace
                   scala-mode
                   ;; scion-mode ;; TODO...
                   starter-kit
                   starter-kit-bindings
                   starter-kit-eshell
                   starter-kit-lisp
                   subword
                   tagedit
                   undo-tree
                   winner
                   yaml-mode
                   ;; yari
                   yasnippet

                   color-theme-sanityinc-solarized
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
