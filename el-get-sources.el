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
      '((:name ac-nrepl
               :description "Nrepl completion source for Emacs auto-complete package"
               :type github
               :pkgname "clojure-emacs/ac-nrepl"
               :checkout "0.21"
               :depends (auto-complete cider clojure-mode dash)
               :features ac-nrepl)
        (:name ace-jump-mode
               :type github
               :pkgname "winterTTr/ace-jump-mode"
               :checkout "v2.0")
        (:name agda2-mode
               :type http
               :url "http://code.haskell.org/Agda/src/data/emacs-mode/agda2-mode.el")
        (:name auto-complete
               :website "https://github.com/auto-complete/auto-complete"
               :description "The most intelligent auto-completion extension."
               :type github
               :pkgname "auto-complete/auto-complete"
               :depends (popup fuzzy)
               :features auto-complete-config
               :post-init (progn
                            (add-to-list 'ac-dictionary-directories
                                         (expand-file-name "dict" default-directory))
                            (ac-config-default)))
        (:name cider
               :type github
               :pkgname "clojure-emacs/cider"
               :checkout "v0.5.0")
        (:name clj-refactor
               :type github
               :pkgname "clojure-emacs/clj-refactor.el"
               :checkout "0.12.0"
               :depends (s dash yasnippet paredit multiple-cursors))
        (:name clojure-mode
               :website "https://github.com/clojure-emacs/clojure-mode"
               :description "Emacs support for the Clojure language."
               :type github
               :pkgname "clojure-emacs/clojure-mode")
        (:name col-highlight
               :description "Highlight the current column."
               :type emacsmirror
               :depends vline)
        (:name color-theme-solarized
               :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme"
               :type github
               :pkgname "sellout/emacs-color-theme-solarized"
               ;; This recipe works with both color-theme and custom-theme.
               ;; We depend on `color-theme' always, for simplicity.
               :depends color-theme
               :prepare (progn
                          ;; prepare for `custom-theme'
                          (add-to-list 'custom-theme-load-path default-directory)
                          ;; prepare for `color-theme'
                          (autoload 'color-theme-solarized-light "color-theme-solarized"
                            "color-theme: solarized-light" t)
                          (autoload 'color-theme-solarized-dark "color-theme-solarized"
                            "color-theme: solarized-dark" t)))
        (:name color-theme-tomorrow
               :description "Emacs highlighting using Chris Charles's Tomorrow color scheme"
               :type github
               :pkgname "ccharles/Tomorrow-Theme"
               :depends color-theme
               :prepare (progn
                          (autoload 'color-theme-tomorrow "GNU Emacs/color-theme-tomorrow"
                            "color-theme: tomorrow" t)
                          (autoload 'color-theme-tomorrow-night "GNU Emacs/color-theme-tomorrow"
                            "color-theme: tomorrow-night" t)
                          (autoload 'color-theme-tomorrow-night-eighties "GNU Emacs/color-theme-tomorrow"
                            "color-theme: tomorrow-night-eighties" t)
                          (autoload 'color-theme-tomorrow-night-blue "GNU Emacs/color-theme-tomorrow"
                            "color-theme: tomorrow-night-blue" t)
                          (autoload 'color-theme-tomorrow-night-bright "GNU Emacs/color-theme-tomorrow"
                            "color-theme: tomorrow-night-bright" t)))
        (:name diff-hl
               :type github
               :pkgname "dgutov/diff-hl"
               :description "Highlights uncommited changes in fringe"
               :checkout "32951f067bdd6f1c1161428c68b81472cc540b8d"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name elixir
               :tpye github
               :pkgname "elixir-lang/emacs-elixir"
               :checkout "v1.4.0")
        (:name expand-region
               :type github
               :pkgname "magnars/expand-region.el"
               :description "Expand region increases the selected region by semantic units. Just keep pressing the key until it selects what you want."
               :checkout "0.10.0")
        (:name fill-column-indicator
               :type github
               :description "An Emacs minor mode that graphically indicates the fill column."
               :pkgname "alpaker/Fill-Column-Indicator"
               :checkout "v1.81")
        (:name floobits
               :type github
               :checkout "1.5.9"
               :pkgname "Floobits/floobits-emacs")
        (:name flycheck-hdevtools
               :type github
               :pkgname "flycheck/flycheck-hdevtools"
               :description "A Flycheck checker for Haskell using hdevtools"
               :checkout "0.3"
               :depends (flycheck)
               :prepare (eval-after-load 'flycheck
                          '(require 'flycheck-hdevtools)))
        (:name fuzzy
               :description "Fuzzy matching utilities for GNU Emacs"
               :type github
               :pkgname "auto-complete/fuzzy-el"
               :checkout "0.1")
        (:name fuzzy-match)
        (:name ghci-completion
               :description "Completion for GHCi commands in inferior-haskell buffers"
               :type github
               :pkgname "manzyuk/ghci-completion"
               :checkout "17cbcbe02bc0cce0094f0e064879fd4b352790d3")
        (:name ghc-mod
               :description "Happy Haskell programming"
               :type github
               :pkgname "kazu-yamamoto/ghc-mod"
               :checkout "v5.0.1"
               :load-path "elisp")
        (:name gist
               :type github
               :pkgname "defunkt/gist.el"
               :depends (gh tabulated-list)
               :checkout "v1.1.0")
        (:name git-timemachine
               :type github
               :pkgname "pidu/git-timemachine"
               :checkout "1.3")
        (:name guru-mode
               :type github
               :pkgname "bbatsov/guru-mode"
               :checkout "v0.2")
        (:name haskell-mode
               :description "A Haskell editing mode"
               :type github
               :pkgname "haskell/haskell-mode"
               :checkout "v13.07"
               :info "."
               :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
               :post-init (progn
                            (require 'haskell-mode-autoloads)
                            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
        (:name highlight-parentheses
               :type github
               :pkgname "nschum/highlight-parentheses.el"
               :checkout "1.0.2")
        (:name highlight-symbol
               :description "Quickly highlight a symbol throughout the buffer and cycle through its locations."
               :type github
               :pkgname "nschum/highlight-symbol.el"
               :checkout "1.2")
        (:name hl-sexp
               :description "Highlight the current sexp"
               :type http
               :url "http://edward.oconnor.cx/elisp/hl-sexp.el"
               :features hl-sexp)
        (:name idle-highlight-mode
               :type github
               :pkgname "nonsequitur/idle-highlight-mode"
               :checkout "1.1.3")
        (:name ido-ubiquitous
               :type github
               :pkgname "DarwinAwardWinner/ido-ubiquitous"
               :checkout "v2.14")
        (:name idomenu
               :type emacswiki
               :description "imenu tag selection a la ido"
               :load-path ".")
        (:name iy-go-to-char
               :type github
               :pkgname "doitian/iy-go-to-char"
               :checkout "v3.2.1")
        (:name key-chord
               :type github
               :pkgname "emacsmirror/key-chord"
               :checkout "972b1672da6d0546f3318d2fa687f4a637a6b22c")
        (:name magit
	       :website "https://github.com/magit/magit#readme"
	       :description "It's Magit! An Emacs mode for Git."
	       :type github
	       :checkout "1.2.1"
	       :pkgname "magit/magit"
	       :depends (cl-lib git-modes s)
	       :info "."
	       ;; let el-get care about autoloads so that it works with all OSes
	       :build (if (version<= "24.3" emacs-version)
			  `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
			`(("make" ,(format "EMACS=%s" el-get-emacs) "docs")))
	       :build/berkeley-unix (("touch" "`find . -name Makefile`") ("gmake"))
	       :prepare (require 'magit-autoloads))
        (:name markdown-mode)
        (:name mic-paren
               :description "Advanced highlighting of matching parentheses")
        (:name multiple-cursors
               :type github
               :pkgname "magnars/multiple-cursors.el"
               :checkout "1.3.0")
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
        (:name paredit)
        (:name projectile
               :type github
               :pkgname "bbatsov/projectile"
               :checkout "v0.11.0")
        (:name protobuf-mode)
        (:name quick-run
               :type github
               :pkgname "syohex/emacs-quickrun"
               :checkout "2.0.3")
        (:name restclient
               :type github
               :pkgname "pashky/restclient.el"
               :checkout "8a3b6eb7439a08df62596561986637d02046cf4d")
        (:name smeargle
               :type github
               :pkgname "syohex/emacs-smeargle"
               :checkout "490e5f5c00d3d6bf2febcba3382e4e619fa0f38e")
        (:name smex
               :type github
               :pkgname "nonsequitur/smex"
               :checkout "3.0")
        (:name sqlup
               :type github
               :pkgname "Trevoke/sqlup-mode.el"
               :checkout "f3aa418bad9aa694956e19344d3de10b3f9930b2")
        (:name tagedit
               :description "A collection of paredit-like fns for use in html-mode")
        (:name undo-tree
               :after (progn
                        (global-undo-tree-mode)))
        (:name wgrep
               :type github
               :pkgname "mhayashi1120/Emacs-wgrep"
               :checkout "8e91b932d9c64c5525a1c0a8c770ec55e213e790")
        (:name workgroups)  ;; https://github.com/pashinin/workgroups2 also!
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
