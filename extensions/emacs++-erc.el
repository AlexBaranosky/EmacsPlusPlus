(package-require 'erc)
(package-require 'erc-log)
(package-require 'erc-notify)
(package-require 'erc-spelling)
(package-require 'erc-autoaway)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; open query buffers in the current window
(setq erc-query-display 'buffer)

;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(setq erc-save-buffer-on-part t)

;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(erc-spelling-mode 1)

(defun irc-alert (nick msg)
  (if (eq system-type 'linux)
      (shell-command-to-string
       (format "notify-send -u critical '%s says:' '%s'" nick msg))
    (message
     (format "%s says: %s" nick msg))))

(defun notify-on-text-nick-match (matched-type nick erc-message)
  (let* ((nick (car (split-string nick "!")))
         (msg (mapconcat 'identity (cdr (split-string erc-message ": ")) " ")))
    (irc-alert nick msg)))

(add-hook 'erc-text-matched-hook 'notify-on-text-nick-match)

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from
the same person.")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

;; private message notification
(defun erc-notify-on-private-msg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (erc-notify-allowed-p nick))
      (irc-alert nick msg)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-on-private-msg)

;; autoaway setup
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-use-emacs-idle t)

(setq erc-auto-reconnect t)


;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

(defcustom erc-nick nil
  "Nick to use for ERC"
  :group 'emacs++
  :type string)

(defun start-erc ()
  (interactive)
  (if erc-nick
      (erc :server "irc.freenode.net" :port 6667 :nick erc-nick)
    (message "Can't start ERC, no nick set")))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-erc ()
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))

;;;

(provide 'emacs++-erc)
