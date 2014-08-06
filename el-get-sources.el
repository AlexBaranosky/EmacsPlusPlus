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
      '((:name cider)
	(:name magit)
	(:name org-mode)
	(:name smex)
	(:name projectile)
	(:name clojure-mode)
	(:name markdown-mode)
	(:name wgrep)
	(:name clj-refactor)
	(:name ido-ubiquitous)
	(:name ac-nrepl)
	(:name ace-jump-mode)
	(:name auto-complete)
	(:name emmet-mode)
	(:name etags-select)
	(:name expand-region)
	(:name fill-column-indicator)
	(:name flycheck-hdevtools)
	(:name fuzzy)
	(:name ghc-mod)
	(:name ghci-completion)
	(:name gist)
	(:name go-mode)
	(:name guru-mode)
	(:name haskell-mode)
	(:name highlight-parentheses)
	(:name highlight-symbol)
	(:name hl-sexp)
	(:name idle-highlight-mode)
	(:name twittering-mode)
	(:name idomenu)
	(:name inf-ruby)
	(:name key-chord)
	(:name multiple-cursors)
	(:name nav)
	(:name projectile)
	(:name protobuf-mode)
	(:name robe-mode)
	(:name ruby-compilation)
	(:name ruby-mode)
	(:name undo-tree)
	(:name yaml-mode)
	(:name yasnippet)
	(:name color-theme-solarized)
	(:name color-theme-tomorrow)
	(:name col-highlight)
	(:name fuzzy-match)
	(:name iy-go-to-char)
	(:name mic-paren)
	(:name tagedit)
	(:name workgroups)
        ;; (:name exec-path-from-shell
        ;;        :type github
        ;;        :pkgname "purcell/exec-path-from-shell"
        ;;        :checkout "1.7")
        (:name elixir
               :tpye github
               :pkgname "elixir-lang/emacs-elixir"
               :checkout "v1.4.0")
        (:name clj-refactor
               :type github
               :pkgname "clojure-emacs/clj-refactor.el"
               :checkout "0.12.0"
               :depends (s dash yasnippet paredit multiple-cursors))
        ;; (:name cider
        ;;        :type github
        ;;        :pkgname "clojure-emacs/cider"
        ;;        :checkout "v0.5.0" ;;"a7bf35fbcaa027d9906eb8e7c13e87293a38ac05"
        ;;        )
        (:name idle-highlight-mode
               :type github
               :pkgname "nonsequitur/idle-highlight-mode"
               :checkout "1.1.3")
        (:name smeargle
               :type github
               :pkgname "syohex/emacs-smeargle"
               :checkout "490e5f5c00d3d6bf2febcba3382e4e619fa0f38e")
        (:name quick-run
               :type github
               :pkgname "syohex/emacs-quickrun"
               :checkout "2.0.3")
        (:name restclient
               :type github
               :pkgname "pashky/restclient.el"
               :checkout "8a3b6eb7439a08df62596561986637d02046cf4d")
        (:name sqlup
               :type github
               :pkgname "Trevoke/sqlup-mode.el"
               :checkout "f3aa418bad9aa694956e19344d3de10b3f9930b2")
        (:name diff-hl
               :type elpa)
        (:name paradox
               :type github
               :pkgname "Bruce-Connor/paradox"
               :checkout "0.10")
        (:name git-timemachine
               :type github
               :pkgname "pidu/git-timemachine"
               :checkout "1.3")
        (:name agda2-mode
               :type http
               :url "http://code.haskell.org/Agda/src/data/emacs-mode/agda2-mode.el")
        ;; (:name multicolumn
        ;;        :type github
        ;;        :pkgname "Lindydancer/multicolumn"
        ;;        :checkout "9cf6ea89ebda6adeece47067eee3beb3a983f6c9")
        ))

;; (idle-highlight-mode +1)

;; LOST PACKAGES:
;; saveplace
;; malabar-mode

(setq my-packages (mapcar 'el-get-source-name el-get-sources))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)

(when window-system
  (require 'package))
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;           '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d")
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-refresh-first-time nil)
(defun package-require (package-name)
  "tries to require. If it fails, it retrieves the package and
   tries to require again (y benzap on #emacs)"
  (if (not (require package-name nil t))
      (progn
        (if (not package-refresh-first-time)
            (save-excursion
              (package-refresh-contents)
              (setq package-refresh-contents t)))
        (package-install package-name)
        (require package-name nil t))))

;; (package-require 'idris-mode)
;; (package-require 'jabber)
