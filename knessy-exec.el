;; -*- lexical-binding: t; -*-

;; TODO (pgu, 29.05.2026): need to add the container selection code as well

;; shamelessly taken from kubel
(defun knessy--setup-tramp ()
  "Setup a kubectl TRAMP."
  (setq tramp-methods (delete (assoc "kubectl" tramp-methods) tramp-methods)) ;; cleanup previous tramp method
  ;; TODO error message if resource is not pod
  (add-to-list 'tramp-methods
               `("kubectl"
                 (tramp-login-program      ,kubel-kubectl)
                 (tramp-login-args         (,(kubel--get-context-namespace) ("exec" "-it") ("-c" "%u") ("%h") ("--" "sh")))
                 (tramp-remote-shell       "sh")
                 (tramp-remote-shell-args  ("-i" "-c")))))

(provide 'knessy-exec)
