
;; add `setq-local' and `user-error' for emacs <= 24.2,
;; as `projectile' requires them

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val))
  )

(unless (fboundp 'user-error)
  (defun user-error (format &rest args)
    "Signal a pilot error, making error message by passing all args to `format'.
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency.
This is just like `error' except that `user-error's are expected to be the
result of an incorrect manipulation on the part of the user, rather than the
result of an actual problem."
    (signal 'user-error (list (apply #'format format args))))
  )

