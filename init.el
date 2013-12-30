;;;; References

;; https://gist.github.com/benzap/2895437

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


;;;; Component Functions

(defun setup-packaging-system ()
  (when window-system
    (require 'package))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)

  ;; MELPAs not necessarily stable code
  ;; (add-to-list 'package-archives
  ;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
  (add-hook 'clojure-mode 'clojure-test-mode))

(defun setup-starter-kits-packages ()
  (package-require 'starter-kit)
  (package-require 'starter-kit-bindings)
  (package-require 'starter-kit-eshell)
  (package-require 'starter-kit-lisp))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun setup-packages-needing-tweaks ()
  (package-require 'key-chord)
  (key-chord-mode 1)

  (global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
  (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-C-<down>")  'shrink-window)
  (global-set-key (kbd "S-C-<up>")    'enlarge-window)

  (package-require 'expand-region)

  (defun cursor-jump-up ()
    (interactive)
    (forward-line -15))

  (defun cursor-jump-down ()
    (interactive)
    (forward-line 15))

  (key-chord-define-global "jj" 'ace-jump-word-mode)
  (key-chord-define-global "jl" 'ace-jump-line-mode)
  (key-chord-define-global "jc" 'ace-jump-char-mode)
  (key-chord-define-global "gg" 'goto-line)
  (key-chord-define-global "zz" 'repeat)
  (key-chord-define-global "--" 'switch-to-previous-buffer)
  (key-chord-define-global "gp" 'rgrep)
  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "ii" 'cursor-jump-up)
  (key-chord-define-global "kk" 'cursor-jump-down)  
  
  (global-set-key (kbd "C-c C-r") 'rename-sgml-tag)

  (package-require 'auto-complete)
  (global-auto-complete-mode t)

  (package-require 'jabber)
  (setq jabber-nickname "Alex Baranosky")
  (setq jabber-account-list
        '(("alexander.baranosky@gmail.com"
           (:network-server . "talk.google.com")
           (:connection-type . ssl))))

  (package-require 'robe)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'robe-ac-setup)

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

  ;; (package-require 'js2-mode)
  ;; (package-require 'js2-refactor)
  ;; (js2r-add-keybindings-with-prefix "C-c C-t")

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
  ;; (package-require 'color-theme-sanityinc-solarized)

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
  (package-require 'slamhound)
  (package-require 'projectile)
  (package-require 'protobuf-mode)
  ;; (package-require 'clues-theme)
  ;;(package-require 'cyberpunk-theme)
  ;; (package-require 'deep-thought-theme)
  ;;(package-require 'twilight-theme)
  (package-require 'vline)
  ;; (package-require 'zenburn-theme)
)

(defun setup-assorted-emacs ()
  (add-to-list
   'auto-mode-alist
   '("\\.edn\\'" . clojure-mode))
  (add-to-list
   'auto-mode-alist
   '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list
   'auto-mode-alist
   '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

  (when (file-exists-p "~/.zshrc")
    ;; Get zshrc into PATH
    (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
      (setenv "PATH" path)
      (setq exec-path
            (append
             (split-string-and-unquote path ":")
             exec-path))))

  (global-auto-revert-mode t)
  ;; (global-linum-mode)
  (setq clojure-font-lock-comment-sexp t)
  (setq-default fill-column 80)
  (when (string-equal system-type "darwin")
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'meta)
    (set-default-font "-apple-inconsolata-medium-r-normal--14-180-72-72-m-180-iso8859-1"))
  
  
  (setq ring-bell-function (lambda () (message "*beep*")))
  ;; (load-theme 'zenburn)
  (load-theme 'sanityinc-solarized-dark)
  ;; (load-theme 'twilight-theme)
  ;; (load-theme 'clues)
  ;;(load-theme 'cyberpunk)
  ;; (load-theme 'deep-thought)

  
  (setq erc-auto-reconnect t)
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#clojure" "#emacs" "#bitcoin")))

  ;; ;; TODO: make alexbaranosky
  (when (y-or-n-p "Do you want to start ERC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick "abaranosky")))

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

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
;; (add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "C-x g") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" magit-diff-options)
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (add-to-list 'magit-diff-options "-w")
;;   (magit-refresh))

;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (setq magit-diff-options (remove "-w" magit-diff-options))
;;   (magit-refresh))

;;;; Load 'Em Up!

(setup-packaging-system)

(setup-starter-kits-packages)
(setup-clojure-packages)
(setup-packages-needing-tweaks)
(setup-remaining-packages)
(setup-assorted-emacs)

(define-tag-functions)
(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "CLJ")

;; (hack-clojure-test-mode-for-gui-diff)

;; (define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)
(setq magit-highlight-whitespace nil)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(define-key paredit-mode-map (kbd "M-)")
  'paredit-wrap-round-from-behind)
;; (key-chord-define-global "dp" 'paredit-duplicate-closest-sexp)



(defun cider-p-eval-last-sexp ()
  "Evaluate the expression preceding point and `p` its value in a popup buffer."
  (interactive)
  (let ((form (cider-last-sexp))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (format "(gui.diff/p %s)" form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(define-key cider-mode-map (kbd "C-c C-q") 'cider-p-eval-last-sexp)

;;;; Emacs Auto-Added Code, below

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes (quote ("246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
