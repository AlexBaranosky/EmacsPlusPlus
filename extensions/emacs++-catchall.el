(global-auto-complete-mode t)
(show-paren-mode 1)
(subword-mode 1)
(winner-mode)

;; (add-hook 'prog-mode-hook (lambda ()
;;                             (guru-mode +1)))

(defun g-blame ()
  (interactive)
  (shell-command
   (format "git show $(git blame '%s' -L %s,%s | awk '{print $1}')"
           (buffer-file-name)
           (line-number-at-pos)
           (line-number-at-pos))))

(defun g-churn ()
  (interactive)
  (shell-command
   "set -e; git log --all -M -C --name-only --format='format:' \"--since='6 months ago'\" | sort | grep -v '^$' | uniq -c | sort | awk 'BEGIN {print \"count\tfile\"} {print $1 \"\t\" $2}' | sort -g | tail -50"))

(defun g-who ()
  (interactive)
  (shell-command
   (concat "git log --format='%an' --since='6 months ago' "
           (buffer-file-name)
           " | sort | uniq -c | sort -rn")))

(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(key-chord-mode 1)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jc" 'ace-jump-char-mode)
(key-chord-define-global "ii" 'cursor-jump-up)
(key-chord-define-global "kk" 'cursor-jump-down)
(key-chord-define-global ",." 'er/expand-region)
(key-chord-define-global "qq" 'rotate-windows)
(key-chord-define-global "ww" 'wgrep-change-to-wgrep-mode)
(key-chord-define-global "xx" 'gui-diff-last-failure)
(key-chord-define-global "uu" 'sqlup-capitalize-keywords-in-region)
(key-chord-define-global "zz" 'workspace-goto)

(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))

(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; (js2r-add-keybindings-with-prefix "C-c C-t")

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

(setq twittering-use-master-password t)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)
(setq emmet-move-cursor-after-expanding nil) 

(global-set-key (kbd "<S-up>")    'windmove-up)
(global-set-key (kbd "<S-down>")  'windmove-down)
(global-set-key (kbd "<S-left>")  'windmove-left)
(global-set-key (kbd "<S-right>") 'windmove-right)

(idle-highlight-mode +1)
(workgroups-mode 1)
(wg-load "~/.emacs.d/emacs_workgroups")

;; (require 'breadcrumb)
;; (global-set-key [(shift space)] 'bc-set) ;; Shift-SPACE for set bookmark
;; (global-set-key [(meta j)] 'bc-previous) ;; M-j for jump to previous
;; (global-set-key [(shift meta j)] 'bc-next) ;; Shift-M-j for jump to next
;; (global-set-key [(meta up)] 'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key [(meta down)] 'bc-local-next) ;; M-down-arrow for local next
;; (global-set-key [(control c)(j)] 'bc-goto-current) ;; C-c j for jump to current bookmark
;; (global-set-key [(control x)(meta j)] 'bc-list) ;; C-x M-j for the bookmark menu list

(provide 'emacs++-catchall)

