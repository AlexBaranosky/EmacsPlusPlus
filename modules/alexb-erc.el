(alexb-package-require 'erc)

(setq erc-auto-reconnect t)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#emacs" "#bitcoin")))

(defun alexb-start-erc ()
  (interactive)
  ;; ;; TODO: make alexbaranosky
  (when (y-or-n-p "Do you want to start ERC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick "abaranosky")))

(alexb-start-erc)

(provide 'alexb-erc)

