;;* ruby
;;** code folding
(defun bmz/ruby-mode-init-folding ()
  (setq outline-regexp " *\\(def \\|class\\|module\\)")

  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (arg) (ruby-end-of-block))
                 nil
                 ))
  )

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'bmz/ruby-mode-init-folding)
     ))


;;** inf-ruby
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
         ))


;;** code completion
;;...





;;** misc
;;*** ruby-block
(autoload 'ruby-block-mode  "ruby-block"
  "In ruby-mode, Displays the line where there is keyword corresponding" t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'ruby-block-mode)

;;*** rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
