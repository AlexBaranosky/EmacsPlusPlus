(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(add-hook 'clojure-mode 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-t")))

(defun cljr-cycle-privacy ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\((defn-? \\)\\|\\((def \\)")
    (cond
     ((looking-at "(defn-")
      (forward-char 5)
      (delete-char 1))
     ((looking-at "(defn")
      (forward-char 5)
      (insert "-"))
     ((looking-at "(def ^:private")
      (forward-char 5)
      (delete-char 10))
     ((looking-at "(def ")
      (forward-char 5)
      (insert "^:private ")))))

(define-key clojure-mode-map (kbd "C-x C-r") 'cljr-rename-file)
(key-chord-define-global "p[" 'cljr-cycle-privacy)
(define-key clojure-mode-map (kbd "C->") 'cljr-thread)
(define-key clojure-mode-map (kbd "C-<") 'cljr-unwind)

(define-key clojure-mode-map (kbd "M-C->") 'cljr-thread-first-all)
(define-key clojure-mode-map (kbd "M-C-?") 'cljr-thread-last-all)

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun toggle-clj-keyword-string ()
  "convert the string or keyword at (point) from string->keyword or keyword->string."
  (interactive)
  (let* ((original-point (point)))
    (while (and
            (> (point) 1)
            (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake."))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (live-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (live-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and
            (> (point) 1)
            (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal "#{" (buffer-substring-no-properties (point) (+ 2 (point)))))
            (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))

    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))

     ((equal "#" (buffer-substring-no-properties (point) (+ 1 (point))))
      (progn
        (delete-char 1)
        (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")")))
     
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (if (equal ?# (char-before))
          (progn
            (backward-char)
            (delete-char 1)
            (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
        (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]")))
     
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "#{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
     
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-:") 'toggle-clj-keyword-string)
(define-key clojure-mode-map (kbd "C-;") 'cycle-clj-coll)

;; Warn about missing nREPL instead of doing stupid things

(defun alexb-nrepl-warn-when-not-connected ()
  (interactive)
  (message "Oops! You're not connected to an nREPL server. Please run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-z") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-k") 'alexb-nrepl-warn-when-not-connected)
(define-key clojure-mode-map (kbd "C-c C-n") 'alexb-nrepl-warn-when-not-connected)


(provide 'alexb-clojure)
