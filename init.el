;;;; References

;; https://gist.github.com/benzap/2895437 


;;;; Component Functions

(defun setup-packaging-system ()
  (when window-system
    (require 'package))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'load-path "~/.emacs.d")
  (package-initialize)
  
  (when (not package-archive-contents)
    (package-refresh-contents)))

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

(defun setup-cider ()
  (package-require 'cider)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
  (add-hook 'nrepl-connected-hook
            (defun pnh-clojure-mode-eldoc-hook ()
              (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
              (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode) ;;?
              (cider-enable-on-existing-clojure-buffers)))
  (add-hook 'cider-mode-hook 'subword-mode)
  (package-require 'ac-nrepl)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode))
  (add-hook 'cider-mode-hook 'ac-nrepl-setup))

(defun setup-clojure-packages ()
  (package-require 'clojure-mode)
  (setup-cider)
  (package-require 'clojure-test-mode)
  (add-hook 'clojure-mode 'paredit-mode)
  (add-hook 'clojure-mode 'packclojure-test-mode))

(defun setup-starter-kits-packages ()
  (package-require 'starter-kit)
  (package-require 'starter-kit-bindings)
  (package-require 'starter-kit-eshell)
  (package-require 'starter-kit-lisp)
  (package-require 'starter-kit-ruby))

(defun setup-packages-needing-tweaks ()
  (package-require 'key-chord)
  (key-chord-mode 1)

  (global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
  (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-C-<down>")  'shrink-window)
  (global-set-key (kbd "S-C-<up>")    'enlarge-window)
  
  (package-require 'expand-region)
  (key-chord-define-global "jj" 'ace-jump-mode)
  (key-chord-define-global "gg" 'goto-line)
  (key-chord-define-global "zz" 'repeat)
  (key-chord-define-global "gp" 'rgrep)
  
  (package-require 'iy-go-to-char)
  ;;  (global-set-key (kbd "M-m") 'iy-go-to-char)
  (key-chord-define-global ";d" 'backward-kill-word)

  (global-set-key (kbd "C-c C-r") 'rename-sgml-tag)
  
  (global-set-key (kbd "M-j") 'backward-char)
  (global-set-key (kbd "M-l") 'forward-char)
  (global-set-key (kbd "M-i") 'previous-line)
  (global-set-key (kbd "M-k") 'next-line)

  (defun cursor-jump-up ()
    (interactive)
    (forward-line -4))
  
  (global-set-key (kbd "s-<up>") 'cursor-jump-up)

  (defun cursor-jump-down ()
    (interactive)
    (forward-line 4))
  
  (global-set-key (kbd "s-<down>") 'cursor-jump-down)
  
  (package-require 'auto-complete)
  (global-auto-complete-mode t)
 
  (package-require 'jabber)
  (setq jabber-nickname "Alex Baranosky")
  (setq jabber-account-list
        '(("alexander.baranosky@gmail.com" 
           (:network-server . "talk.google.com")
           (:connection-type . ssl))))

  (package-require 'subword)
  (subword-mode)
  
  (package-require 'winner)
  (winner-mode)

  (package-require 'textmate)
  (global-set-key (kbd "C-x M-f") 'textmate-goto-file)

  (package-require 'idomenu)
  (defvar push-mark-before-goto-char nil)
  (defadvice goto-char (before push-mark-first activate)
    (when push-mark-before-goto-char
      (push-mark)))
  (defun ido-imenu-push-mark ()
    (interactive)
    (let ((push-mark-before-goto-char t))
      (idomenu)))
  (global-set-key (kbd "C-x C-i") 'idomenu)
  
  (package-require 'multiple-cursors)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-!") 'mc/mark-all-like-this)
  
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t)

  (package-require 'yasnippet)
  (setq yas/prompt-functions '(yas/ido-prompt
                               yas/completing-prompt))
  (yas/global-mode 1)
  (yas/load-directory "~/.emacs.d/snippets/clojure-mode")
  (yas/load-directory "~/.emacs.d/snippets/org-mode")
  (key-chord-define-global "yy" 'yas-insert-snippet)
  
  (package-require 'clj-refactor)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-t")))
  
  (show-paren-mode 1)
 
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  
  (package-require 'ido-hacks)
  
  (package-require 'undo-tree)
  (global-undo-tree-mode))

(defun setup-remaining-packages ()
  (package-require 'ace-jump-mode)
  (package-require 'all)
  (package-require 'alpha)
  (package-require 'cc-mode)
  (package-require 'cl)
  (package-require 'col-highlight)
  (package-require 'compile)
  (package-require 'color-theme-sanityinc-solarized)
  (package-require 'crosshairs)
  (package-require 'dash)
  (package-require 'etags-select)
  (package-require 'etags-table)
  (package-require 'fill-column-indicator)
  (package-require 'fuzzy)
  (package-require 'fuzzy-match)
  (package-require 'go-mode)
  (package-require 'haskell-mode)
  (package-require 'highlight-parentheses)
  (package-require 'highlight-symbol)
  (package-require 'highline)
  (package-require 'hl-sexp)
  (package-require 'idle-highlight)
  (package-require 'rainbow-delimiters)
  (package-require 'markdown-mode)
  (package-require 'maxframe)
  (package-require 'mic-paren)
  (package-require 'nav)
  (package-require 's)
  (package-require 'scala-mode)
  (package-require 'projectile)
  (package-require 'protobuf-mode)
  (package-require 'ruby-end)
  (package-require 'vline)
  (package-require 'zenburn-theme))

(defun setup-assorted-emacs ()
  (when (file-exists-p "~/.zshrc")
    ;; Get zshrc into PATH
    (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
      (setenv "PATH" path)
      (setq exec-path 
            (append
             (split-string-and-unquote path ":")
             exec-path))))
  
  (global-auto-revert-mode t)
  (global-linum-mode)
  (setq clojure-font-lock-comment-sexp t)
  (setq-default fill-column 90)
  (when (string-equal system-type "darwin")
    (set-default-font "-apple-inconsolata-medium-r-normal--14-180-72-72-m-180-iso8859-1"))
  (setq ring-bell-function (lambda () (message "*beep*")))
  ;; (load-theme 'zenburn t)
  (load-theme 'sanityinc-solarized-dark)

  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#clojure" "#emacs" "#typed-clojure")))

  ;; TODO: make alexbaranosky
  (erc :server "irc.freenode.net" :port 6667 :nick "abaranosky"))

(defun define-tag-functions ()
  (defun my-ido-find-tag ()
    "Find a tag using ido"
    (interactive)
    (tags-completion-table)
    (let (tag-names)
      (mapatoms (lambda (x)
                  (push (prin1-to-string x t) tag-names))
                tags-completion-table)
      (etags-select-find (ido-completing-read "Tag: " tag-names))))

  (setq tags-table-list '("~/Dropbox/repos/clojure/TAGS"))
  (key-chord-define-global "./" 'my-ido-find-tag)
  (key-chord-define-global "m," 'pop-tag-mark)

  ;; For if I want different file types to use different TAG files
  ;; (setq tag-table-alist 
  ;;       '(("clj$|edn$" . "~/Dropbox/repos/clojure/TAGS")
  ;;         ;; (...)
  ;;         ))
  )

(defun hack-clojure-test-mode-for-gui-diff ()
  (defun clojure-test-run-tests ()
    "Run all the tests in the current namespace using gui.diff/run-tests++."
    (interactive)
    (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
    (message "Testing w/ Gui Diff...")
    (if (not (clojure-in-tests-p))
        (cider-load-file (buffer-file-name)))
    (save-window-excursion
      (if (not (clojure-in-tests-p))
          (clojure-jump-to-test))
      (clojure-test-clear)
      (clojure-test-eval (format "(require '[gui.diff]) ;; only changes are in this string.
                                  (gui.diff/run-tests++ '%s)"
                                 (clojure-find-ns))
                         #'clojure-test-get-results))))

;;;; Load 'Em Up!

(setup-packaging-system)

(setup-starter-kits-packages)
(setup-clojure-packages)
(setup-packages-needing-tweaks)
(setup-remaining-packages)
(setup-assorted-emacs)

(define-tag-functions)
(hack-clojure-test-mode-for-gui-diff)


;;;; Emacs Auto-Added Code, below

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
