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
      '((:name ac-nrepl)
        (:name ace-jump-mode)
        (:name agda2-mode
               :type http
               :url "http://code.haskell.org/Agda/src/data/emacs-mode/agda2-mode.el")
        (:name auto-complete)
        (:name cider
               :type github
               :pkgname "clojure-emacs/cider"
               :checkout "v0.5.0")
        (:name clj-refactor
               :type github
               :pkgname "clojure-emacs/clj-refactor.el"
               :checkout "0.12.0"
               :depends (s dash yasnippet paredit multiple-cursors))
        (:name clojure-mode)
        (:name col-highlight)
        (:name color-theme-solarized)
        (:name color-theme-tomorrow)
        (:name diff-hl
               :type github
               :pkgname "dgutov/diff-hl"
               :checkout "32951f067bdd6f1c1161428c68b81472cc540b8d")
        (:name elixir
               :tpye github
               :pkgname "elixir-lang/emacs-elixir"
               :checkout "v1.4.0")
        (:name expand-region)
        (:name fill-column-indicator)
        (:name floobits
               :type github
               :checkout "1.5.9"
               :pkgname "Floobits/floobits-emacs")
        (:name flycheck-hdevtools)
        (:name fuzzy)
        (:name fuzzy-match)
        (:name ghci-completion)
        (:name ghc-mod)
        (:name gist)
        (:name git-timemachine
               :type github
               :pkgname "pidu/git-timemachine"
               :checkout "1.3")
        (:name guru-mode)
        (:name haskell-mode)
        (:name highlight-parentheses)
        (:name highlight-symbol
               :description "Quickly highlight a symbol throughout the buffer and cycle through its locations."
               :type github
               :pkgname "nschum/highlight-symbol.el"
               :checkout "1.2")
        (:name hl-sexp)
        (:name idle-highlight-mode
               :type github
               :pkgname "nonsequitur/idle-highlight-mode"
               :checkout "1.1.3")
        (:name ido-ubiquitous)
        (:name idomenu)
        (:name inf-ruby)
        (:name iy-go-to-char)
        (:name key-chord)
        (:name magit
               :type github
               :checkout "1.2.1"
               :pkgname "magit/magit")
        (:name markdown-mode)
        (:name mic-paren
	       :description "Advanced highlighting of matching parentheses")
        (:name multiple-cursors)
        (:name org-mode
               :after (progn
                        (add-hook 'org-mode-hook
                                  (lambda ()
                                    (org-indent-mode t)
                                    (org-toggle-inline-images)
                                    (toggle-truncate-lines)
                                    (turn-on-font-lock)
                                    (define-key org-mode-map "\C-cl" 'org-store-link)
                                    (define-key org-mode-map "\C-ca" 'org-agenda)
                                    (define-key org-mode-map "\C-cb" 'org-iswitchb)
                                    t))))
        (:name paradox
               :type github
               :pkgname "Bruce-Connor/paradox"
               :checkout "0.10")
        (:name projectile)
        (:name protobuf-mode)
        (:name quick-run
               :type github
               :pkgname "syohex/emacs-quickrun"
               :checkout "2.0.3")
        (:name restclient
               :type github
               :pkgname "pashky/restclient.el"
               :checkout "8a3b6eb7439a08df62596561986637d02046cf4d")
        (:name robe-mode)
        (:name ruby-compilation)
        (:name ruby-mode)
        (:name smeargle
               :type github
               :pkgname "syohex/emacs-smeargle"
               :checkout "490e5f5c00d3d6bf2febcba3382e4e619fa0f38e")
        (:name smex)
        (:name sqlup
               :type github
               :pkgname "Trevoke/sqlup-mode.el"
               :checkout "f3aa418bad9aa694956e19344d3de10b3f9930b2")
        (:name tagedit
	       :description "A collection of paredit-like fns for use in html-mode")
        (:name undo-tree)
        (:name wgrep)
        (:name workgroups  ;; https://github.com/pashinin/workgroups2 also!
               :type github
               :pkgname "tlh/workgroups.el"
               :checkout "9572b3492ee09054dc329f64ed846c962b395e39")
        (:name yaml-mode)
        (:name yasnippet)))

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


;; (:name exec-path-from-shell
;;        :type github
;;        :pkgname "purcell/exec-path-from-shell"
;;        :checkout "1.7")

;; (:name multicolumn
;;        :type github
;;        :pkgname "Lindydancer/multicolumn"
;;        :checkout "9cf6ea89ebda6adeece47067eee3beb3a983f6c9")
