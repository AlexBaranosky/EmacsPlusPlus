(global-auto-complete-mode t)


(add-hook 'prog-mode-hook (lambda ()
                            (guru-mode +1)))

(key-chord-mode 1)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jc" 'ace-jump-char-mode)
(key-chord-define-global "gg" 'goto-line)
(key-chord-define-global "zz" 'repeat)
(key-chord-define-global "uu" 'switch-to-previous-buffer)
(key-chord-define-global "gp" 'rgrep)
(key-chord-define-global "xx" 'smex)
(key-chord-define-global "ii" 'cursor-jump-up)
(key-chord-define-global "kk" 'cursor-jump-down)
(key-chord-define-global ",." 'er/expand-region)
(key-chord-define-global "dc" 'iy-go-to-char-backward)
(key-chord-define-global "fv" 'iy-go-to-char)
(key-chord-define-global "gm" 'guru-mode)

(setq jabber-nickname "Alex Baranosky")
(setq jabber-account-list
      '(("alexander.baranosky@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))          

(subword-mode)
(winner-mode)
(global-set-key (kbd "C-x M-f") 'projectile-find-file)
(defvar push-mark-before-goto-char nil)
(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))
(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (idomenu)))
(global-set-key (kbd "C-x C-i") 'idomenu)

(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt))
(yas/global-mode 1)
(yas/load-directory "~/.emacs.d/snippets/clojure-mode")
(yas/load-directory "~/.emacs.d/snippets/org-mode")
(key-chord-define-global "yy" 'yas-insert-snippet)

;; (js2r-add-keybindings-with-prefix "C-c C-t")

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(global-undo-tree-mode)

;; Save point position between sessions
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "CLJ")
;; (rename-modeline "emacs-lisp-mode" emacs-lisp-mode "ELisp") ;; TODO: does it work?
  
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list
 'auto-mode-alist
 '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))


;; (load-theme 'zenburn)
(load-theme 'sanityinc-solarized-dark)
;; (load-theme 'twilight-theme)
;; (load-theme 'clues)
;;(load-theme 'cyberpunk)
;; (load-theme 'deep-thought)

