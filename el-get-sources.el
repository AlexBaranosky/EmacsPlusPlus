(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes/elpa")

(setq el-get-verbose t)

(setq el-get-sources
      '())

;; LOST PACKAGES:
;; saveplace
;; malabar-mode

(setq my-packages
      (append
       '(
         buffer-move
	 magit
	 org-mode
	 smex
	 projectile
	 clojure-mode
	 cider
	 paredit
	 markdown-mode
	 wgrep
	 clj-refactor
	 ido-ubiquitous
	 ac-nrepl
	 ace-jump-mode
	 auto-complete
	 emmet-mode
	 etags-select
	 expand-region
	 fill-column-indicator
	 flycheck-hdevtools
	 fuzzy
	 ghc-mod
	 ghci-completion
	 gist
	 go-mode
	 guru-mode
	 haskell-mode
	 highlight-parentheses
	 highlight-symbol
	 hl-sexp
	 idle-highlight-mode
	 twittering-mode
	 idomenu
	 inf-ruby
	 key-chord
	 multiple-cursors
	 nav
	 projectile
	 protobuf-mode
	 robe-mode
	 ruby-compilation
	 ruby-mode
	 undo-tree
	 yaml-mode
	 yasnippet
	 color-theme-solarized
	 color-theme-tomorrow

	 col-highlight
	 fuzzy-match
	 iy-go-to-char
	 mic-paren
	 ruby-hash-syntax
	 ;; starter-kit
	 ;; starter-kit-bindings
	 ;; starter-kit-eshell
	 ;; starter-kit-lisp
	 tagedit
	 jabber)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)
