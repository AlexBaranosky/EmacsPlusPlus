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
      '(
        (:name clj-refactor
	       :type elpa
	       :repo nil
	       :depends (s dash yasnippet paredit multiple-cursors))
	(:name cider
               :type github
               :pkgname "clojure-emacs/cider"
               :checkout "v0.5.0" ;;"a7bf35fbcaa027d9906eb8e7c13e87293a38ac05"
               :depends (dash clojure-mode pkg-info))
	(:name idle-highlight-mode
	       :type github
	       :pkgname "nonsequitur/idle-highlight-mode"
	       :checkout "1.1.3")

        ;; (:name cider
        ;;        :type github
        ;;        :pkgname "clojure-emacs/cider"
        ;;        :checkout "v0.6.0" ;;"a7bf35fbcaa027d9906eb8e7c13e87293a38ac05"
        ;;        :depends (dash clojure-mode pkg-info))
	;; (:name cider-nrepl
        ;;        :type github
        ;;        :pkgname "clojure-emacs/cider-nrepl"
        ;;        :checkout "v0.6.0" ;; "7f120899b8ff485e9469add459be094da0321c50"
	;;        )

	))

;; LOST PACKAGES:
;; saveplace
;; malabar-mode

(setq my-packages
      (append
       '(
         magit
         org-mode
         smex
         projectile
         clojure-mode
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
         tagedit
         jabber)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)
