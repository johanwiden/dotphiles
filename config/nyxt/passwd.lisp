(in-package #:nyxt-user)

;; (defmethod initialize-instance :after
;;            ((interface password:password-store-interface)
;;             &key &allow-other-keys)
;;   "This method is only needed if password-store dir is not provided by
;; environment variable PASSWORD_STORE_DIR and is not
;; $HOME/.password-store/"
;;   (setf (password:password-directory interface)
;;           "/home/jw/.password-store"))

;; (defmethod initialize-instance :after ((interface password:keepassxc-interface) &key &allow-other-keys)
;;   "I use KeePassXC, and this simply sets the location of the password files."
;;   (setf (password:password-file interface) "/home/jw/Passwords.kdbx"))

(define-configuration nyxt/mode/password:password-mode
  ((nyxt/mode/password:password-interface
    (make-instance 'password:password-store-interface))))

(define-configuration buffer
  ((default-modes
    (append (list 'nyxt/mode/password:password-mode) %slot-value%))))
